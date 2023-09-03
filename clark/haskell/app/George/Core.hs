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

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.State.Strict
import Data.Bool
import Data.ByteString qualified as B
import Data.Foldable
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
import System.Directory qualified as Dir
import System.Exit
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

newtype AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    }
    deriving (Generic)

data Event where
    ActionEvent :: (Show a) => (a -> IO ()) -> (CompoundAction a) -> Event
    LogEvent :: Text -> Event
    ErrorEvent :: Error -> Event

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error
catchIO :: (MonadCatch m, MonadError Error m) => m a -> m a
catchIO = handleIOError $ throwError . Error "IO error when running action"

type CompoundAction a = Eff '[Action] a
data Action a where
    ResetError :: Action ()
    GetLightPower :: Light a -> Action Bool
    SetLightPower :: Light a -> Bool -> Action ()
    GetLightColour :: Light a -> Action HSBK
    SetLightColour :: {light :: Light FullColours, delay :: NominalDiffTime, colour :: HSBK} -> Action ()
    SetLightColourBK :: {lightBK :: Light KelvinOnly, delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> Action () -- TODO we should in principle be allowed to reuse the name `light` for the field - https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
    SetDeskUSBPower :: Bool -> Action ()
    SendEmail :: {subject :: Text, body :: Text} -> Action ()
    SuspendLaptop :: Action ()
    SetOtherLED :: Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
deriving instance Show (Action a)
data Light (c :: LightColours) where
    Ceiling :: Light KelvinOnly
    Lamp :: Light FullColours
deriving instance Show (Light c)
data LightColours = FullColours | KelvinOnly -- TODO use `type data` when available (GHC 9.6)
instance FromHttpApiData (Exists Light) where
    -- TODO is there a way to derive some of this?
    -- if we could do `deriving instance Read (Light NoColour)` that might be a good start
    parseUrlPiece = \case
        "ceiling" -> Right $ Exists Ceiling
        "lamp" -> Right $ Exists Lamp
        s -> Left $ "unknown light name: " <> s

data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , ledOtherPin :: Int
    , emailPipe :: FilePath
    , sshTimeout :: Int
    , getLight :: forall a. Light a -> Device
    , laptopHostName :: Text
    , deskUsbPort :: Int
    , systemLedPipe :: FilePath
    , setLED :: forall m. (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
    }

runAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadError Error m) => ActionOpts -> Action a -> m a
runAction opts@ActionOpts{getLight, setLED {- TODO GHC doesn't yet support impredicative fields -}} = \case
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
    SetOtherLED b -> setLED opts.ledOtherPin b
    SetSystemLEDs b -> writePipe opts.systemLedPipe . showT $ fromEnum b
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr t
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec
    writePipe p t =
        bool (throwError $ SimpleError "Pipe doesn't exist") (liftIO $ T.writeFile p t)
            =<< liftIO (Dir.doesFileExist p)

toggleCeilingLight :: CompoundAction ()
toggleCeilingLight = send . SetLightPower Ceiling . not =<< send (GetLightPower Ceiling)
sleepOrWake :: NominalDiffTime -> Word16 -> CompoundAction ()
sleepOrWake lifxMorningDelay lifxMorningKelvin =
    send (GetLightPower Ceiling) >>= \night@(not -> morning) -> do
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
        send $ SetDeskUSBPower morning
        when night . void $ send SuspendLaptop
  where
    light = Ceiling
