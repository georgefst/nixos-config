module Main (main) where

import GPIO qualified
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
import Data.Bool
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Tuple.Extra hiding (first, second)
import Data.Word
import Lifx.Lan hiding (SetColor)
import Lifx.Lan qualified as Lifx
import MQTT.Meross qualified
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Generic
import RawFilePath
import Servant
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream qualified as S
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream.StreamK qualified as SK (hoist)
import System.Directory qualified as Dir
import System.Exit
import System.IO
import Text.Pretty.Simple

data Opts = Opts
    { gpioChip :: Text
    , buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , ceilingLightName :: Text
    , lifxTimeout :: Double
    , lifxPort :: Word16
    , receivePort :: Word16
    , httpPort :: Warp.Port
    , emailPipe :: FilePath
    , laptopHostName :: Text
    , sshTimeout :: Int
    , lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    , deskUsbPort :: Int
    , systemLedPipe :: FilePath
    , rootCmdPipe :: FilePath
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

type AppState = Map Int GPIO.Handle
data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
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

    -- TODO initialisation stuff - encapsulate this better somehow, without it being killed by LIFX failure
    -- TODO even discounting LIFX issue, unclear how to do this in Streamly 0.9, as there's no `Monad (Stream IO)`
    eventSocket <- socket AF_INET Datagram defaultProtocol
    bind eventSocket $ SockAddrInet (fromIntegral opts.receivePort) 0
    eventMVar <- newEmptyMVar
    let gpioMonitor =
            GPIO.mon (encodeUtf8 opts.gpioChip) (putMVar eventMVar . LogEvent) opts.buttonDebounce opts.buttonPin
                . putMVar eventMVar
                $ ActionEvent SleepOrWake

    race_
        ( gpioMonitor
            `race_` Warp.runSettings
                (warpSettings opts.httpPort $ putMVar eventMVar . LogEvent . TL.toStrict . pShow)
                (webServer $ liftIO . putMVar eventMVar)
        )
        . flip evalStateT mempty
        . flip runLoggingT (liftIO . T.putStrLn)
        . runLifxUntilSuccess
            (either (\() -> handleError $ SimpleError "Ceiling light not found") (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
        $ do
            ceilingLight <-
                maybe (throwError ()) (pure . fst)
                    . find ((== opts.ceilingLightName) . (.label) . snd)
                    =<< traverse (\d -> (d,) <$> sendMessage d GetColor)
                    =<< discoverDevices Nothing
            S.fold
                ( SF.drainMapM \case
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
                )
                . (SK.toStream . SK.hoist liftIO . SK.fromStream)
                $ S.parList
                    id
                    [ S.repeatM $
                        either (ErrorEvent . Error "Decode failure") ActionEvent . decodeAction . BSL.fromStrict
                            <$> recv eventSocket 4096
                    , S.repeatM $ takeMVar eventMVar
                    ]

data SimpleAction a where
    ResetError :: SimpleAction ()
    GetCeilingLightPower :: SimpleAction Bool
    SetCeilingLightPower :: Bool -> SimpleAction ()
    SetCeilingLightColour :: {delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> SimpleAction ()
    SetDeskUSBPower :: Bool -> SimpleAction ()
    SendEmail :: {subject :: Text, body :: Text} -> SimpleAction ()
    SuspendLaptop :: SimpleAction ()
    SetSystemLEDs :: Bool -> SimpleAction ()
    RootCommand :: Text -> SimpleAction ()
deriving instance Show (SimpleAction a)
data SimpleActionOpts = SimpleActionOpts
    { ledErrorPin :: Int
    , emailPipe :: FilePath
    , sshTimeout :: Int
    , ceilingLight :: Device
    , laptopHostName :: Text
    , deskUsbPort :: Int
    , systemLedPipe :: FilePath
    , rootCmdPipe :: FilePath
    }

runSimpleAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadError Error m) => SimpleActionOpts -> SimpleAction a -> m a
runSimpleAction opts = \case
    ResetError ->
        gets (Map.lookup opts.ledErrorPin) >>= \case
            Just h -> liftIO (GPIO.reset h) >> modify (Map.delete opts.ledErrorPin)
            Nothing -> liftIO $ putStrLn "LED is already off"
    GetCeilingLightPower -> statePowerToBool <$> sendMessage opts.ceilingLight GetPower
    SetCeilingLightPower p -> sendMessage opts.ceilingLight $ SetPower p
    SetCeilingLightColour{delay, brightness, kelvin} -> sendMessage opts.ceilingLight $ Lifx.SetColor HSBK{..} delay
      where
        -- these have no effect for this type of LIFX bulb
        hue = 0
        saturation = 0
    SetDeskUSBPower b -> do
        (ec, out, err) <- MQTT.Meross.send =<< MQTT.Meross.toggle opts.deskUsbPort b
        showOutput out err
        throwWhenFailureExitCode "Failed to set desk USB power" ec
    SendEmail{subject, body} ->
        writePipe opts.emailPipe $ T.unlines [subject, body]
    SuspendLaptop ->
        maybe
            (throwError $ SimpleError "SSH timeout")
            (throwWhenFailureExitCode "SSH failure")
            =<< liftIO
                ( traverse (\(e, out, err) -> showOutput out err >> pure e)
                    <=< readProcessWithExitCodeTimeout (opts.sshTimeout * 1_000_000)
                    $ proc "ssh" ["billy", "systemctl suspend"]
                )
    SetSystemLEDs b -> writePipe opts.systemLedPipe . showT $ fromEnum b
    RootCommand t -> writePipe opts.rootCmdPipe t
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr t
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec
    writePipe p t =
        bool (throwError $ SimpleError "Pipe doesn't exist") (liftIO $ T.writeFile p t)
            =<< liftIO (Dir.doesFileExist p)

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
            when morning do
                send
                    SetCeilingLightColour
                        { delay = 0
                        , brightness = 0
                        , kelvin = 0
                        }
                send
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
            8 -> SimpleAction . SetSystemLEDs <$> B.get @Bool
            -- actions which wouldn't be useful to run remotely/standalone
            255 -> pure $ SimpleAction GetCeilingLightPower
            n -> fail $ "unknown action: " <> show n

-- TODO use new record-style Servant approach - it'd be easy to mix up the endpoints here currently
-- although I actually don't think Servant is a perfect fit for this at all
-- really we want to map directly from URLs to `Action`/`SimpleAction ()`s
-- relatedly, when defining server, Servant makes it hard to abstract over the repetition
-- and hard to define handlers as an exhaustive pattern match on actions
-- and it would also be nice if we could use the server as a Streamly stream more directly
-- other frameworks also may make it easier to accept multiple verbs, so we don't have to stick to GET
-- is there some way we can actually return stuff for e.g. `GetCeilingLightPower`, rather than always `NoContent`?
type UserAPI =
    "reset-error" :> GetNoContent
        :<|> "set-ceiling-light-power"
            :> Capture "power" Bool
            :> GetNoContent
        :<|> "set-ceiling-light-colour"
            :> Capture "delay" NominalDiffTime
            :> Capture "brightness" Word16
            :> Capture "kelvin" Word16
            :> GetNoContent
        :<|> "set-desk-usb-power"
            :> Capture "power" Bool
            :> GetNoContent
        :<|> "send-email"
            :> Capture "subject" Text
            :> Capture "body" Text
            :> GetNoContent
        :<|> "suspend-laptop" :> GetNoContent
        :<|> "set-system-leds"
            :> Capture "power" Bool
            :> GetNoContent
        :<|> "toggle-light" :> GetNoContent
        :<|> "sleep-or-wake" :> GetNoContent
webServer :: (forall m. (MonadIO m) => Event -> m ()) -> Application
webServer f =
    serve (Proxy @UserAPI) $
        (f (ActionEvent $ SimpleAction ResetError) >> r)
            :<|> (\p -> f (ActionEvent $ SimpleAction $ SetCeilingLightPower p) >> r)
            :<|> (\delay brightness kelvin -> f (ActionEvent $ SimpleAction $ SetCeilingLightColour{..}) >> r)
            :<|> (\p -> f (ActionEvent $ SimpleAction $ SetDeskUSBPower p) >> r)
            :<|> (\subject body -> f (ActionEvent $ SimpleAction $ SendEmail{..}) >> r)
            :<|> (f (ActionEvent $ SimpleAction SuspendLaptop) >> r)
            :<|> (\p -> f (ActionEvent $ SimpleAction $ SetSystemLEDs p) >> r)
            :<|> (f (ActionEvent ToggleLight) >> r)
            :<|> (f (ActionEvent SleepOrWake) >> r)
  where
    r = pure NoContent
warpSettings ::
    Warp.Port ->
    (forall a. (Show a) => a -> IO ()) ->
    Warp.Settings
warpSettings port logger =
    Warp.setLogger (curry3 $ unless . statusIsSuccessful . snd3 <*> logger)
        . Warp.setPort port
        $ Warp.defaultSettings

data Event
    = ActionEvent Action
    | LogEvent Text
    | ErrorEvent Error
