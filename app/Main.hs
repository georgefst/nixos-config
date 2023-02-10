module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Freer.Writer qualified as Eff
import Control.Monad.Log (logMessage, runLoggingT)
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
import Lifx.Lan qualified as Lifx
import MQTT.Meross qualified
import Network.HTTP.Client
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Network.Wreq hiding (get, put)
import Options.Generic
import RawFilePath
import Streamly.Internal.Data.Stream.IsStream qualified as S (hoist)
import Streamly.Prelude qualified as S
import System.Exit
import System.IO
import Text.Pretty.Simple
import Util.Lifx

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
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

type AppState = Map Int (Process Inherit Inherit Inherit)
data Error where
    Error :: Show a => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Clark"
    let handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            gets (Map.member opts.ledErrorPin) >>= \case
                False -> modify . Map.insert opts.ledErrorPin =<< gpioSet [opts.ledErrorPin]
                True -> liftIO $ putStrLn "LED is already on"
        -- TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
        light = deviceFromAddress (192, 168, 1, 190)

    -- TODO initialisation stuff - encapsulate this better somehow, without it being killed by LIFX failure
    eventSocket <- socket AF_INET Datagram defaultProtocol
    bind eventSocket $ SockAddrInet (fromIntegral opts.receivePort) 0
    gpioEventMVar <- newEmptyMVar
    let gpioMonitor =
            gpioMon (putMVar gpioEventMVar . LogEvent) opts.buttonDebounce opts.buttonPin
                . putMVar gpioEventMVar
                $ ActionEvent SleepOrWake

    race_ gpioMonitor
        . flip evalStateT mempty
        . flip runLoggingT (liftIO . T.putStrLn)
        . runLifxUntilSuccess (lifxTime opts.lifxTimeout)
        . S.mapM_ \case
            ErrorEvent e -> handleError e
            LogEvent t -> logMessage t
            ActionEvent action ->
                (either handleError pure <=< runExceptT)
                    . (logMessage . snd @() <=< runM)
                    . translate (runSimpleAction (opts & \Opts{..} -> ActionOpts{..}))
                    . Eff.runWriter
                    $ do
                        case action of
                            SimpleAction a -> Eff.tell $ showT a
                            a -> Eff.tell $ showT a
                        raise $ runAction action
        . S.hoist liftIO
        . S.fromAsync
        $ mconcat
            [ S.repeatM $
                either (ErrorEvent . Error "Decode failure") ActionEvent . decodeAction . BSL.fromStrict
                    <$> recv eventSocket 4096
            , S.repeatM $ takeMVar gpioEventMVar
            ]

data SimpleAction a where
    ResetError :: SimpleAction ()
    ToggleLight :: SimpleAction Bool -- returns `True` if the light is _now_ on
    SetLightColour :: NominalDiffTime -> Word16 -> Word16 -> SimpleAction () -- brightness and temp
    SetDeskUSBPower :: Bool -> SimpleAction ()
    SendEmail :: {subject :: Text, body :: Text} -> SimpleAction (Response BSL.ByteString)
    SuspendBilly :: SimpleAction ()
deriving instance Show (SimpleAction a)
data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , mailgunSandbox :: Text
    , mailgunKey :: Text
    , sshTimeout :: Int
    , light :: Device
    }

runSimpleAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadError Error m) => ActionOpts -> SimpleAction a -> m a
runSimpleAction opts = \case
    ResetError ->
        gets (Map.lookup opts.ledErrorPin) >>= \case
            Just h -> liftIO (terminateProcess h) >> modify (Map.delete opts.ledErrorPin)
            Nothing -> liftIO $ putStrLn "LED is already off"
    ToggleLight -> do
        r <- not . statePowerToBool <$> sendMessage opts.light GetPower
        sendMessage opts.light $ SetPower r
        pure r
    SetLightColour time brightness kelvin -> sendMessage opts.light $ Lifx.SetColor HSBK{..} time
      where
        -- these have no effect for this type of LIFX bulb
        hue = 0
        saturation = 0
    SetDeskUSBPower b -> do
        (ec, out, err) <- MQTT.Meross.send =<< MQTT.Meross.toggle 2 b
        showOutput out err
        throwWhenFailureExitCode "Failed to set desk USB power" ec
    SendEmail{subject, body} ->
        either (throwError . Error "Failed to send email") pure
            =<< sendEmail (opts & \ActionOpts{..} -> EmailOpts{..})
    SuspendBilly ->
        maybe
            (throwError $ SimpleError "SSH timeout")
            (throwWhenFailureExitCode "SSH failure")
            =<< liftIO
                ( traverse (\(e, out, err) -> showOutput out err >> pure e)
                    <=< readProcessWithExitCodeTimeout (opts.sshTimeout * 1_000_000)
                    $ proc "ssh" ["billy", "systemctl suspend"]
                )
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr t
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec

data Action where
    SimpleAction :: SimpleAction a -> Action
    SleepOrWake :: Action
deriving instance Show Action
runAction :: Action -> Eff '[SimpleAction] ()
runAction = \case
    SimpleAction a -> void $ send a
    SleepOrWake ->
        send ToggleLight >>= \morning@(not -> night) -> do
            when morning $ send $ SetLightColour 45 maxBound 2700
            send $ SetDeskUSBPower morning
            when night . void $ send SuspendBilly
decodeAction :: BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) Action
decodeAction =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ SimpleAction ResetError
            1 -> pure $ SimpleAction ToggleLight
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ SimpleAction SendEmail{..}
            3 -> pure $ SimpleAction SuspendBilly
            4 -> SimpleAction . SetDeskUSBPower . (/= 0) <$> B.get @Word8
            5 -> SimpleAction <$> (SetLightColour . secondsToNominalDiffTime <$> B.get <*> B.get <*> B.get)
            6 -> pure SleepOrWake
            n -> fail $ "unknown action: " <> show n

data Event
    = ActionEvent Action
    | LogEvent Text
    | ErrorEvent Error

{- Util -}

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

-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
gpioSet :: MonadIO m => [Int] -> m (Process Inherit Inherit Inherit)
gpioSet xs =
    liftIO
        . startProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . showBS) xs
gpioMon :: (Text -> IO ()) -> Double -> Int -> IO () -> IO ()
gpioMon putLine debounce pin x = do
    p <-
        startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, showBS pin]
                `setStdout` CreatePipe
    getCurrentTime >>= iterateM_ \t0 -> do
        line <- hGetLine $ processStdout p
        t1 <- getCurrentTime
        if diffUTCTime t1 t0 < realToFrac debounce
            then putLine $ "(Ignoring) " <> T.pack line
            else putLine (T.pack line) >> x
        pure t1
gpioChip :: ByteString
gpioChip = "gpiochip0"

showT :: Show a => a -> Text
showT = T.pack . show
showBS :: Show a => a -> ByteString
showBS = encodeUtf8 . showT

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
