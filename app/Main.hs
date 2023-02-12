module Main (main) where

import GPIO qualified
import Mailgun qualified
import Util
import Util.Lifx

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Freer.Writer qualified as Eff
import Control.Monad.Log (logMessage, runLoggingT)
import Control.Monad.State
import Data.Binary qualified as B
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Get qualified as B
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
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
import Options.Generic
import RawFilePath
import Streamly.Internal.Data.Stream.IsStream qualified as S (hoist)
import Streamly.Prelude qualified as S
import System.Exit
import System.IO
import Text.Pretty.Simple

data Opts = Opts
    { gpioChip :: Text
    , buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , lightName :: Text
    , lifxTimeout :: Double
    , receivePort :: Word16
    , mailgunSandbox :: Text
    , mailgunKey :: Text
    , emailAddress :: Text
    , laptopHostName :: Text
    , sshTimeout :: Int
    , lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    , deskUsbPort :: Int
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

type AppState = Map Int GPIO.Handle
data Error where
    Error :: Show a => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Clark"
    let handleError :: (MonadIO m, MonadState AppState m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            gets (Map.member opts.ledErrorPin) >>= \case
                False -> modify . Map.insert opts.ledErrorPin =<< GPIO.set (encodeUtf8 opts.gpioChip) [opts.ledErrorPin]
                True -> liftIO $ putStrLn "LED is already on"
        -- TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
        light = deviceFromAddress (192, 168, 1, 190)

    -- TODO initialisation stuff - encapsulate this better somehow, without it being killed by LIFX failure
    eventSocket <- socket AF_INET Datagram defaultProtocol
    bind eventSocket $ SockAddrInet (fromIntegral opts.receivePort) 0
    gpioEventMVar <- newEmptyMVar
    let gpioMonitor =
            GPIO.mon (encodeUtf8 opts.gpioChip) (putMVar gpioEventMVar . LogEvent) opts.buttonDebounce opts.buttonPin
                . putMVar gpioEventMVar
                $ ActionEvent SleepOrWake

    race_ gpioMonitor
        . flip evalStateT mempty
        . flip runLoggingT (liftIO . T.putStrLn)
        . runLifxUntilSuccess (handleError . Error "LIFX error") (lifxTime opts.lifxTimeout)
        . S.mapM_ \case
            ErrorEvent e -> handleError e
            LogEvent t -> logMessage t
            ActionEvent action ->
                (either handleError pure <=< runExceptT)
                    . (logMessage . snd @() <=< runM)
                    . translate (runSimpleAction (opts & \Opts{..} -> SimpleActionOpts{..}))
                    . Eff.runWriter
                    $ do
                        case action of
                            SimpleAction a -> Eff.tell $ showT a
                            a -> Eff.tell $ showT a
                        raise $ runAction (opts & \Opts{..} -> ActionOpts{..}) action
        . S.hoist liftIO
        . S.fromAsync
        $ mconcat
            [ S.fromSerial . S.repeatM $
                either (ErrorEvent . Error "Decode failure") ActionEvent . decodeAction . BSL.fromStrict
                    <$> recv eventSocket 4096
            , S.fromSerial . S.repeatM $ takeMVar gpioEventMVar
            ]

data SimpleAction a where
    ResetError :: SimpleAction ()
    GetCeilingLightPower :: SimpleAction Bool
    SetCeilingLightPower :: Bool -> SimpleAction ()
    SetCeilingLightColour :: {delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> SimpleAction ()
    SetDeskUSBPower :: Bool -> SimpleAction ()
    SendEmail :: {subject :: Text, body :: Text} -> SimpleAction (Response BSL.ByteString)
    SuspendLaptop :: SimpleAction ()
    SetSystemLEDs :: Bool -> SimpleAction ()
deriving instance Show (SimpleAction a)
data SimpleActionOpts = SimpleActionOpts
    { ledErrorPin :: Int
    , mailgunSandbox :: Text
    , mailgunKey :: Text
    , sshTimeout :: Int
    , light :: Device
    , emailAddress :: Text
    , laptopHostName :: Text
    , deskUsbPort :: Int
    }

runSimpleAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadError Error m) => SimpleActionOpts -> SimpleAction a -> m a
runSimpleAction opts = \case
    ResetError ->
        gets (Map.lookup opts.ledErrorPin) >>= \case
            Just h -> liftIO (GPIO.reset h) >> modify (Map.delete opts.ledErrorPin)
            Nothing -> liftIO $ putStrLn "LED is already off"
    GetCeilingLightPower -> statePowerToBool <$> sendMessage opts.light GetPower
    SetCeilingLightPower p -> sendMessage opts.light $ SetPower p
    SetCeilingLightColour{delay, brightness, kelvin} -> sendMessage opts.light $ Lifx.SetColor HSBK{..} delay
      where
        -- these have no effect for this type of LIFX bulb
        hue = 0
        saturation = 0
    SetDeskUSBPower b -> do
        (ec, out, err) <- MQTT.Meross.send =<< MQTT.Meross.toggle opts.deskUsbPort b
        showOutput out err
        throwWhenFailureExitCode "Failed to set desk USB power" ec
    SendEmail{subject, body} ->
        either (throwError . Error "Failed to send email") pure
            =<< Mailgun.send
                Mailgun.Opts
                    { key = opts.mailgunKey
                    , sandbox = opts.mailgunSandbox
                    , from = opts.emailAddress
                    , to = opts.emailAddress
                    , subject
                    , body
                    }
    SuspendLaptop ->
        maybe
            (throwError $ SimpleError "SSH timeout")
            (throwWhenFailureExitCode "SSH failure")
            =<< liftIO
                ( traverse (\(e, out, err) -> showOutput out err >> pure e)
                    <=< readProcessWithExitCodeTimeout (opts.sshTimeout * 1_000_000)
                    $ proc "ssh" [encodeUtf8 opts.laptopHostName, "systemctl suspend"]
                )
    SetSystemLEDs b -> for_
        [("/sys/class/leds/" <> "mmc1::", "mmc1"), ("/sys/class/leds/" <> "ACT", "heartbeat")]
        \(led, val) -> liftIO $ T.writeFile (led <> "/trigger") if b then val else "none"
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr t
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec

data Action where
    SimpleAction :: SimpleAction a -> Action
    ToggleLight :: Action
    SleepOrWake :: Action
deriving instance Show Action
data ActionOpts = ActionOpts
    { lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    }
runAction :: ActionOpts -> Action -> Eff '[SimpleAction] ()
runAction opts = \case
    SimpleAction a -> void $ send a
    ToggleLight -> send . SetCeilingLightPower . not =<< send GetCeilingLightPower
    SleepOrWake ->
        send GetCeilingLightPower >>= \night@(not -> morning) -> do
            send $ SetSystemLEDs morning
            send $ SetCeilingLightPower morning
            when morning . send $
                SetCeilingLightColour
                    { delay = fromIntegral opts.lifxMorningSeconds
                    , brightness = maxBound
                    , kelvin = opts.lifxMorningKelvin
                    }
            send $ SetDeskUSBPower morning
            when night . void $ send SuspendLaptop
decodeAction :: BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) Action
decodeAction =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ SimpleAction ResetError
            1 -> pure ToggleLight
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ SimpleAction SendEmail{..}
            3 -> pure $ SimpleAction SuspendLaptop
            4 -> SimpleAction . SetDeskUSBPower <$> B.get @Bool
            5 -> SimpleAction <$> (SetCeilingLightColour . secondsToNominalDiffTime <$> B.get <*> B.get <*> B.get)
            6 -> pure SleepOrWake
            7 -> SimpleAction . SetCeilingLightPower <$> B.get @Bool
            8 -> pure $ SimpleAction GetCeilingLightPower
            9 -> SimpleAction . SetSystemLEDs <$> B.get @Bool
            n -> fail $ "unknown action: " <> show n

data Event
    = ActionEvent Action
    | LogEvent Text
    | ErrorEvent Error
