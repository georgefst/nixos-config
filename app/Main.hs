module Main (main) where

-- TODO this is in its own section due to a Fourmolu bug with reordering comments in import lists
import Text.Pretty.Simple hiding (Color (..), Intensity (..)) -- TODO https://github.com/quchen/prettyprinter/issues/233

import Compound
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (IOException)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State
import Data.Binary qualified as B
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Get qualified as B
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Either.Extra
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra hiding (first, second)
import Data.Word
import Lifx.Lan hiding (SetColor)
import MQTT.Meross qualified
import Network.HTTP.Client
import Network.Socket
import Network.Socket.ByteString
import Network.Wreq hiding (get, put)
import Options.Generic
import RawFilePath
import System.Console.ANSI
import System.Exit
import System.IO

data Opts = Opts
    { buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , lightName :: Text
    , lifxTimeout :: Double
    , receivePort :: Word16
    , mailgunSandbox :: Text
    , mailgunKey :: Text
    , sshTimeout :: Int
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers
                }

data Action a where
    ResetError :: Action ()
    ToggleLight :: Action Bool -- returns `True` if the light is _now_ on
    SetDeskUSBPower :: Bool -> Action ()
    SendEmail :: {subject :: Text, body :: Text} -> Action (Response BSL.ByteString)
    SuspendBilly :: Action (Maybe ExitCode)
deriving instance Show (Action a)

type AppM x = StateT (Map Int (Process Inherit Inherit Inherit)) (LifxT IO) x

newtype ActionQueue = ActionQueue {unwrap :: MVar ActionOrError}
newActionQueue :: MonadIO m => m ActionQueue
newActionQueue = liftIO $ ActionQueue <$> newEmptyMVar
enqueueError :: (MonadIO m, Show e) => ActionQueue -> Text -> e -> m ()
enqueueError q t = liftIO . putMVar (q.unwrap) . curry Left t . Exists
enqueueAction :: (MonadIO m, Show a) => ActionQueue -> Compound Action a -> m ()
enqueueAction q = liftIO . putMVar (q.unwrap) . Right . Some
dequeueActions :: MonadIO m => ActionQueue -> ((Text, Exists Show) -> m ()) -> (forall a. Compound Action a -> m ()) -> m ()
dequeueActions q h x =
    forever $
        liftIO (takeMVar q.unwrap) >>= \case
            Right (Some a) -> x a
            Left e -> h e
type ActionOrError = Either (Text, Exists Show) (Some (Compound Action))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    opts@Opts{mailgunSandbox, mailgunKey} <- getRecord "Clark"
    queue <- newActionQueue
    -- TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
    let light = deviceFromAddress (192, 168, 1, 192)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral opts.receivePort) 0
            forever do
                bs <- recv sock 4096
                case decodeAction $ BSL.fromStrict bs of
                    Right (Some x) -> enqueueAction queue $ Compound.single x
                    Left e -> enqueueError queue "Decode failure" e

    let listenForButton =
            gpioMon opts.buttonDebounce opts.buttonPin . enqueueAction queue $
                Compound.single ToggleLight >>= \morning -> do
                    Compound.single $ SetDeskUSBPower morning
                    unless morning . void $ Compound.single SuspendBilly

    let handleError :: Show a => Text -> a -> AppM ()
        handleError title body = do
            withSGR' Red $ T.putStrLn $ title <> ":"
            pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
            gets (Map.member opts.ledErrorPin) >>= \case
                False -> modify . Map.insert opts.ledErrorPin =<< gpioSet [opts.ledErrorPin]
                True -> liftIO $ putStrLn "LED is already on"

    listenOnNetwork `concurrently_` listenForButton `concurrently_` runLifxUntilSuccess (lifxTime opts.lifxTimeout) do
        flip evalStateT mempty . dequeueActions queue (\(s, Exists e) -> handleError s e) $
            either (\(s, Exists e) -> handleError s e) (const $ pure ())
                <=< runExceptT @(Text, Exists Show) . Compound.run \action -> do
                    pPrint action -- TODO better logging
                    case action of
                        ResetError ->
                            gets (Map.lookup opts.ledErrorPin) >>= \case
                                Just h -> liftIO (terminateProcess h) >> modify (Map.delete opts.ledErrorPin)
                                Nothing -> liftIO $ putStrLn "LED is already off"
                        ToggleLight -> toggleLight light
                        SetDeskUSBPower b -> do
                            (e, out, err) <- MQTT.Meross.send $ MQTT.Meross.toggle 4 b
                            showOutput out err
                            case e of
                                ExitSuccess -> pure ()
                                ExitFailure n -> throwError ("Failed to set desk USB power", Exists n)
                        SendEmail{subject, body} ->
                            either (throwError . ("Failed to send email",) . Exists) pure
                                =<< sendEmail EmailOpts{..}
                        SuspendBilly ->
                            -- TODO restore error throwing once we have a physical button for `ResetError`
                            -- common up with `SetDeskUSBPower`
                            {- HLINT ignore main "Monad law, right identity" -}
                            pure
                                =<< liftIO
                                    ( traverse (\(e, out, err) -> showOutput out err >> pure e)
                                        <=< readProcessWithExitCodeTimeout (opts.sshTimeout * 1_000_000)
                                        $ proc
                                            "ssh"
                                            [ "-i/home/gthomas/.ssh/id_rsa"
                                            , "-oUserKnownHostsFile=/home/gthomas/.ssh/known_hosts"
                                            , "gthomas@billy"
                                            , "systemctl suspend"
                                            ]
                                    )
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr t

decodeAction :: BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) (Some Action)
decodeAction =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ Some ResetError
            1 -> pure $ Some ToggleLight
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ Some SendEmail{..}
            3 -> pure $ Some SuspendBilly
            n -> fail $ "unknown action: " <> show n

{- Run action -}

toggleLight :: MonadLifx m => Device -> m Bool
toggleLight light = do
    r <- not . statePowerToBool <$> sendMessage light GetPower
    sendMessage light $ SetPower r
    pure r

data EmailOpts = EmailOpts
    { mailgunKey :: Text
    , mailgunSandbox :: Text
    , subject :: Text
    , body :: Text
    }
sendEmail :: MonadIO m => EmailOpts -> m (Either HttpExceptionContent (Response BSL.ByteString))
sendEmail EmailOpts{..} =
    liftIO $ tryHttpException $ postWith postOpts url formParams
  where
    postOpts = defaults & auth ?~ basicAuth "api" (encodeUtf8 mailgunKey)
    url = "https://api.mailgun.net/v3/sandbox" <> T.unpack mailgunSandbox <> ".mailgun.org/messages"
    formParams =
        [ "from" := "Mailgun Sandbox <postmaster@sandbox" <> mailgunSandbox <> ".mailgun.org>"
        , "to" := ("George Thomas <georgefsthomas@gmail.com>" :: Text)
        , "subject" := subject
        , "text" := body
        ]
    tryHttpException = tryJust \case
        HttpExceptionRequest _ e -> Just e
        InvalidUrlException _ _ -> Nothing

{- Util -}

-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
gpioSet :: MonadIO m => [Int] -> m (Process Inherit Inherit Inherit)
gpioSet xs =
    liftIO
        . startProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . showBS) xs
gpioMon :: Double -> Int -> IO () -> IO ()
gpioMon debounce pin x = do
    putStrLn "Starting gpiomon process..."
    p <-
        startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, showBS pin]
                `setStdout` CreatePipe
    let gpiomonStdout = processStdout p
    putStrLn "Done!"

    handle (\(e :: IOException) -> print e >> terminateProcess p >> hClose gpiomonStdout) $
        getCurrentTime >>= iterateM_ \t0 -> do
            line <- hGetLine gpiomonStdout
            t1 <- getCurrentTime
            if diffUTCTime t1 t0 < realToFrac debounce
                then withSGR' Red $ putStr "Ignoring: "
                else do
                    withSGR' Green $ putStr "Ok: "
                    x
            putStrLn line
            pure t1
gpioChip :: ByteString
gpioChip = "gpiochip0"

-- TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

showT :: Show a => a -> Text
showT = T.pack . show
showBS :: Show a => a -> ByteString
showBS = encodeUtf8 . showT

-- TODO lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

-- TODO upstream? not the first time I've defined this
withSGR :: [SGR] -> IO a -> IO a
withSGR sgr x = do
    setSGR sgr
    r <- x
    setSGR [Reset]
    pure r

-- A special case of 'withSGR'
withSGR' :: MonadIO m => Color -> IO a -> m a
withSGR' x = liftIO . withSGR [SetColor Foreground Vivid x, SetConsoleIntensity BoldIntensity]

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: Int -> Lifx a -> IO a
runLifxUntilSuccess t x = either (\e -> print e >> threadDelay t >> runLifxUntilSuccess t x) pure =<< runLifxT t x

-- TODO replace with first-class existential when they arrive (https://github.com/ghc-proposals/ghc-proposals/pull/473)
data Exists c where
    Exists :: c a => a -> Exists c
data Some d where
    Some :: Show a => d a -> Some d

-- TODO return partial stdout/stderr in timeout case

{- | Essentially `\t -> timeout t . readProcessWithExitCode`, except that it actually works since it uses `SIGTERM`,
whereas `timeout` uses an async exception, and thus isn't good at terminating foreign code.
Time is in microseconds, as with `threadDelay` and `timeout`.
-}
readProcessWithExitCodeTimeout :: Int -> ProcessConf stdin stdout stderr -> IO (Maybe (ExitCode, ByteString, ByteString))
readProcessWithExitCodeTimeout t conf = do
    p <-
        startProcess $
            conf
                `setStdin` NoStream
                `setStdout` CreatePipe
                `setStderr` CreatePipe
    eitherToMaybe @() <$> ((threadDelay t >> terminateProcess p) `race` waitForProcess p)
        >>= traverse \exitCode -> (exitCode,,) <$> B.hGetContents (processStdout p) <*> B.hGetContents (processStderr p)
