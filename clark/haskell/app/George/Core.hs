{- TODO this is intended to eventually form the core of a library:
George's
Effective (pun!)
Organiser of
Receiving and
Generating
Events
-}

module George.Core where

import Util
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except hiding (handleError)
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.State.Strict
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC8
import Data.Foldable
import Data.Function
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Word
import Lifx.Lan hiding (SetColor, SetLightPower)
import Lifx.Lan qualified as Lifx
import MQTT.Meross qualified
import Options.Generic
import RawFilePath
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.IO.Error
import Util.Util
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

newtype AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    }
    deriving (Generic)

data Event where
    ActionEvent :: (Show a) => (a -> IO ()) -> (CompoundAction a) -> Event
    LogEvent :: Text -> Event
    ErrorEvent :: Error -> Event
runEventStream ::
    (MonadIO m) =>
    (Error -> m ()) ->
    (Text -> m ()) ->
    (forall a. Action a -> ExceptT Error m a) ->
    S.Stream m [Event] ->
    m ()
runEventStream handleError log' run' =
    S.fold
        ( SF.drainMapM \case
            ErrorEvent e -> handleError e
            LogEvent t -> log' t
            ActionEvent f action -> (either handleError pure <=< runExceptT) $ runM do
                r <-
                    action & translate \a -> do
                        lift . log' $ showT a
                        run' a
                sendM . lift . log' $ showT r
                sendM . liftIO $ f r
        )
        . S.concatMap S.fromList
        . S.cons [LogEvent "Starting..."]

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error

-- TODO what I really want is just to catch all non-async exceptions
-- is there no good way to do this? maybe by catching all then re-throwing asyncs?
-- it does seem to be difficult - https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell
-- TODO on the other hand, should the other exception types used here be made subtypes of `IOException`?
catchActionErrors :: forall m a. (MonadCatch m, MonadError Error m) => m a -> m a
catchActionErrors = catchMany @'[IOException] $ throwError . Error "Error when running action"

type CompoundAction a = Eff '[Action] a
data Action a where
    Exit :: ExitCode -> Action ()
    PowerOff :: Action ()
    ResetError :: Action ()
    GetLightPower :: Light a -> Action Bool
    SetLightPower :: Light a -> Bool -> Action ()
    GetLightColour :: Light a -> Action HSBK
    SetLightColour :: {light :: Light FullColours, delay :: NominalDiffTime, colour :: HSBK} -> Action ()
    SetLightColourBK :: {lightBK :: Light KelvinOnly, delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> Action () -- TODO we should in principle be allowed to reuse the name `light` for the field - https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
    SetDeskPower :: DeskPowerDevice -> Bool -> Action ()
    SendEmail :: {subject :: Text, body :: Text} -> Action ()
    SuspendLaptop :: Action ()
    SetOtherLED :: Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
deriving instance Show (Action a)
data Light (c :: LightColours) where
    Ceiling :: Light KelvinOnly
    Lamp :: Light FullColours
deriving instance Show (Light c)
type data LightColours = FullColours | KelvinOnly
data DeskPowerDevice
    = Computer
    | MainMonitor
    | PortraitMonitor
    | -- | Turning these off is a really bad idea, since they power Clark itself!
      UsbPorts
    deriving (Show, Read)

lightName :: Light c -> Text
lightName = \case
    Ceiling -> "Ceiling"
    Lamp -> "Lamp"

-- TODO is there a way to derive some of this?
-- if we could do `deriving instance Read (Light NoColour)` that might be a good start
instance FromHttpApiData (Exists' Light) where
    parseUrlPiece = \case
        "ceiling" -> Right $ Exists Ceiling
        "lamp" -> Right $ Exists Lamp
        s -> Left $ "unknown light name: " <> s
instance Bounded (Exists' Light) where
    minBound = Exists Ceiling
    maxBound = Exists Lamp
instance Enum (Exists' Light) where
    toEnum = \case
        0 -> Exists Ceiling
        1 -> Exists Lamp
        _ -> error "out of bounds for light enum"
    fromEnum = \case
        Exists Ceiling -> 0
        Exists Lamp -> 1
instance FromHttpApiData (Light KelvinOnly) where
    parseUrlPiece = \case
        "ceiling" -> Right Ceiling
        s -> Left $ "unknown light name: " <> s
instance FromHttpApiData (Light FullColours) where
    parseUrlPiece = \case
        "lamp" -> Right Lamp
        s -> Left $ "unknown light name: " <> s
instance FromHttpApiData DeskPowerDevice where
    parseUrlPiece = \case
        "computer" -> Right Computer
        "main-monitor" -> Right MainMonitor
        "portrait-monitor" -> Right PortraitMonitor
        "usb-ports" -> Right UsbPorts
        s -> Left $ "unknown desk device: " <> s

data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , ledOtherPin :: Int
    , emailPipe :: FilePath
    , sshTimeout :: Int
    , getLight :: forall a. Light a -> Device
    , laptopHostName :: Text
    , systemLedPipe :: FilePath
    , powerOffPipe :: FilePath
    , setLED :: forall m. (MonadState AppState m, MonadLog Text m, MonadIO m) => Int -> Bool -> m ()
    }

runAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadLog Text m, MonadError Error m, MonadCatch m) =>
    ActionOpts ->
    Action a ->
    m a
runAction opts@ActionOpts{getLight, setLED {- TODO GHC doesn't yet support impredicative fields -}} = \case
    Exit c -> liftIO $ exitWith c
    PowerOff -> writePipe opts.powerOffPipe "."
    ResetError -> setLED opts.ledErrorPin False
    GetLightPower l -> statePowerToBool <$> sendMessage (getLight l) GetPower
    SetLightPower l p -> sendMessage (getLight l) $ SetPower p
    GetLightColour l -> (.hsbk) <$> sendMessage (getLight l) Lifx.GetColor
    SetLightColour{..} -> sendMessage (getLight light) $ Lifx.SetColor colour delay
    SetLightColourBK{lightBK = light, ..} -> sendMessage (getLight light) $ Lifx.SetColor HSBK{..} delay
      where
        -- these have no effect for this type of LIFX bulb
        hue = 0
        saturation = 0
    SetDeskPower d b -> do
        (ec, out, err) <- MQTT.Meross.send =<< MQTT.Meross.toggle port b
        showOutput out err
        throwWhenFailureExitCode "Failed to set desk power" ec
      where
        port = case d of
            Computer -> 3
            MainMonitor -> 2
            PortraitMonitor -> 1
            UsbPorts -> 4
    SendEmail{subject, body} ->
        writePipe opts.emailPipe $ T.unlines [subject, body]
    SuspendLaptop ->
        maybe
            (logMessage "SSH timeout")
            (logWhenFailureExitCode "SSH failure")
            =<< liftIO
                ( traverse (\(e, out, err) -> showOutput out err >> pure e)
                    <=< readProcessWithExitCodeTimeout (opts.sshTimeout * 1_000_000)
                    $ proc "ssh" ["billy", "systemctl suspend"]
                )
    SetOtherLED b -> setLED opts.ledOtherPin b
    SetSystemLEDs b -> writePipe opts.systemLedPipe . showT $ fromEnum b
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr (BC8.strip t)
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec
    logWhenFailureExitCode s = \case
        ExitSuccess -> pure ()
        ExitFailure ec -> logMessage $ s <> ": " <> showT ec
    writePipe p t =
        liftIO (T.writeFile p t)
            `catchDNE` \_ ->
                throwError $ SimpleError "Pipe doesn't exist"
      where
        catchDNE = catchIf isDoesNotExistError

toggleLight :: Light a ->CompoundAction ()
toggleLight l = send . SetLightPower l . not =<< send (GetLightPower l)
sleepOrWake :: NominalDiffTime -> Word16 -> CompoundAction ()
sleepOrWake lifxMorningDelay lifxMorningKelvin =
    send (GetLightPower light) >>= \night@(not -> morning) -> do
        send $ SetSystemLEDs morning
        send $ SetLightPower light morning
        when morning do
            send
                SetLightColourBK
                    { lightBK = light
                    , delay = 0
                    , brightness = 0
                    , kelvin = 0
                    }
            send
                SetLightColourBK
                    { lightBK = light
                    , delay = lifxMorningDelay
                    , brightness = maxBound
                    , kelvin = lifxMorningKelvin
                    }
        when night . void $ send SuspendLaptop
  where
    light = Ceiling
