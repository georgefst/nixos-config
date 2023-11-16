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
import Control.Monad.Log (MonadLog)
import Control.Monad.State.Strict
import Data.Bool
import Data.ByteString qualified as B
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
import System.Directory qualified as Dir
import System.Exit
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

-- TODO is there a way to derive some of this?
-- if we could do `deriving instance Read (Light NoColour)` that might be a good start
instance FromHttpApiData (Exists' Light) where
    parseUrlPiece = \case
        "ceiling" -> Right $ Exists Ceiling
        "lamp" -> Right $ Exists Lamp
        s -> Left $ "unknown light name: " <> s
instance FromHttpApiData (Light KelvinOnly) where
    parseUrlPiece = \case
        "ceiling" -> Right Ceiling
        s -> Left $ "unknown light name: " <> s
instance FromHttpApiData (Light FullColours) where
    parseUrlPiece = \case
        "lamp" -> Right Lamp
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
    , powerOffPipe :: FilePath
    , setLED :: forall m. (MonadState AppState m, MonadLog Text m, MonadIO m) => Int -> Bool -> m ()
    }

runAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadLog Text m, MonadError Error m) => ActionOpts -> Action a -> m a
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
