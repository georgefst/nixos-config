{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

-- TODO this is in its own section due to a Fourmolu bug with reordering comments in import lists
import Text.Pretty.Simple hiding (Color (..), Intensity (..)) -- TODO https://github.com/quchen/prettyprinter/issues/233

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
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra hiding (first, second)
import Data.Word
import Lifx.Lan hiding (SetColor)
import Lifx.Lan.Internal (LifxT (LifxT))
import Network.HTTP.Client
import Network.Socket
import Network.Socket.ByteString
import Network.Wreq hiding (get, put)
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra
import System.Timeout (timeout)

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
    SendEmail :: {subject :: Text, body :: Text} -> Action ()
    SuspendBilly :: Action ()
deriving instance Show (Action a)

-- TODO add these instances upstream (and/or remove the `MonadState` instance so that we don't even need opaque `AppM`)
deriving newtype instance MonadThrow (LifxT IO)
deriving newtype instance MonadCatch (LifxT IO)
newtype AppM x = AppM {unwrap :: StateT (Map Int ProcessHandle) (LifxT IO) x}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState (Map Int ProcessHandle)
        , MonadLifx
        , MonadThrow
        , MonadCatch
        )

-- TODO look in to how this relates to free monads
data PerformActions a where
    CompoundAction :: PerformActions a -> (a -> PerformActions b) -> PerformActions b
    OneAction :: Action a -> PerformActions a
    SimpleAction :: a -> PerformActions a
performAction :: Action a -> PerformActions a
performAction = OneAction
performActions :: Monad m => (forall r. Action r -> m (Either e r)) -> PerformActions a -> m (Either e a)
performActions run = \case
    CompoundAction m f -> performActions run m >>= either (pure . Left) (performActions run . f)
    OneAction a -> run a
    SimpleAction x -> pure $ Right x
instance Functor PerformActions where
    fmap = (<*>) . pure
instance Applicative PerformActions where
    pure = SimpleAction
    (<*>) = ap
instance Monad PerformActions where
    (>>=) = CompoundAction

newtype ActionQueue = ActionQueue {unwrap :: MVar ActionOrError}
newActionQueue :: MonadIO m => m ActionQueue
newActionQueue = liftIO $ ActionQueue <$> newEmptyMVar
enqueueError :: (MonadIO m, Show e) => ActionQueue -> Text -> e -> m ()
enqueueError q t = liftIO . putMVar (q.unwrap) . curry Left t . Exists
enqueueAction :: (MonadIO m, Show a) => ActionQueue -> PerformActions a -> m ()
enqueueAction q = liftIO . putMVar (q.unwrap) . Right . Some
dequeueActions :: MonadIO m => ActionQueue -> ((Text, Exists Show) -> m ()) -> (forall a. PerformActions a -> m ()) -> m ()
dequeueActions q h x =
    forever $
        liftIO (takeMVar q.unwrap) >>= \case
            Right (Some a) -> x a
            Left e -> h e
type ActionOrError = Either (Text, Exists Show) (Some PerformActions)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    opts@Opts{mailgunSandbox, mailgunKey} <- getRecord "Clark"
    queue <- newActionQueue
    -- TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
    let light = deviceFromAddress (192, 168, 1, 190)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral opts.receivePort) 0
            forever do
                bs <- recv sock 4096
                case decodeAction $ BSL.fromStrict bs of
                    Right (Some x) -> enqueueAction queue $ performAction x
                    Left e -> enqueueError queue "Decode failure" e

    let listenForButton =
            gpioMon opts.buttonDebounce opts.buttonPin . enqueueAction queue $
                performAction ToggleLight >>= flip unless (performAction SuspendBilly)

    let handleError :: Show a => Text -> a -> AppM ()
        handleError title body = do
            withSGR' Red $ T.putStrLn $ title <> ":"
            pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
            gets (Map.lookup opts.ledErrorPin) >>= \case
                Nothing -> modify . Map.insert opts.ledErrorPin =<< gpioSet [opts.ledErrorPin]
                Just h -> liftIO $ putStrLn . ("LED is already on: pid " <>) . maybe "not found" show =<< getPid h

    listenOnNetwork `concurrently_` listenForButton `concurrently_` runLifxUntilSuccess (lifxTime opts.lifxTimeout) do
        flip evalStateT mempty . (.unwrap) . dequeueActions @AppM queue (\(s, Exists e) -> handleError s e) $
            either (\(s, Exists e) -> handleError s e) (const $ pure ())
                <=< performActions \action -> runExceptT @(Text, Exists Show) do
                    pPrint action -- TODO better logging
                    case action of
                        ResetError ->
                            gets (Map.lookup opts.ledErrorPin) >>= \case
                                Just h -> liftIO (terminateProcess h) >> modify (Map.delete opts.ledErrorPin)
                                Nothing -> liftIO $ putStrLn "LED is already off"
                        ToggleLight -> toggleLight light
                        SendEmail{subject, body} ->
                            sendEmail EmailOpts{..}
                                >>= either (throwError . ("Failed to send email",) . Exists) pure
                        SuspendBilly ->
                            maybe (throwError ("SSH timeout", Exists ())) pure
                                =<< liftIO
                                    ( timeout
                                        (opts.sshTimeout * 1_000_000)
                                        ( callProcess
                                            "ssh"
                                            [ "-i/home/gthomas/.ssh/id_rsa"
                                            , "-oUserKnownHostsFile=/home/gthomas/.ssh/known_hosts"
                                            , "gthomas@billy"
                                            , "systemctl suspend"
                                            ]
                                        )
                                    )

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
sendEmail :: MonadIO m => EmailOpts -> m (Either HttpExceptionContent ())
sendEmail EmailOpts{..} =
    liftIO $
        (Right () <$ postWith postOpts url formParams)
            `catchHttpException` (pure . Left)
  where
    postOpts = defaults & auth ?~ basicAuth "api" (encodeUtf8 mailgunKey)
    url = "https://api.mailgun.net/v3/sandbox" <> T.unpack mailgunSandbox <> ".mailgun.org/messages"
    formParams =
        [ "from" := "Mailgun Sandbox <postmaster@sandbox" <> mailgunSandbox <> ".mailgun.org>"
        , "to" := ("George Thomas <georgefsthomas@gmail.com>" :: Text)
        , "subject" := subject
        , "text" := body
        ]
    catchHttpException = catchJust \case
        HttpExceptionRequest _ e -> Just e
        InvalidUrlException _ _ -> Nothing

{- Util -}

-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
gpioSet :: MonadIO m => [Int] -> m ProcessHandle
gpioSet xs =
    liftIO
        . fmap (\(_, _, _, x) -> x)
        . createProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . show) xs
gpioMon :: Double -> Int -> IO () -> IO ()
gpioMon debounce pin x = do
    putStrLn "Starting gpiomon process..."
    hs@(_, Just gpiomonStdout, _, _) <-
        createProcess
            (proc "gpiomon" ["-b", "-f", gpioChip, show pin])
                { std_out = CreatePipe
                }
    putStrLn "Done!"

    handle (\(e :: IOException) -> print e >> cleanupProcess hs) $
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
gpioChip :: String
gpioChip = "gpiochip0"

-- TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

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
