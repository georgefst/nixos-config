{-# LANGUAGE TemplateHaskell #-}
{- TODO this is intended to eventually form the core of a library:
George's
Effective (pun!)
Organiser of
Receiving and
Generating
Events
-}
{-# LANGUAGE UndecidableInstances #-}

module George.Core where

import Util
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except hiding (handleError)
import Control.Monad.Freer
import Control.Monad.Freer.Error qualified as Freer
import Control.Monad.Freer.TH
import Control.Monad.Log (MonadLog)
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC8
import Data.Foldable
import Data.Functor
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Typeable
import Data.Word
import Lifx.Lan hiding (SetColor, SetLightPower)
import Lifx.Lan qualified as Lifx
import MQTT.Meross qualified
import Options.Generic
import Servant.Foreign
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.IO.Error
import Util.Servant.Curl
import Util.Util

newtype AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    }
    deriving (Generic)

data Log r where
    Log :: Text -> Log ()
logMsg :: (Member Log effs) => Text -> Eff effs ()
logMsg = send . Log
type Program a = Eff '[Action, Log, Freer.Error Error] a
type CompoundAction a = Eff '[Action] a
type Event = Program ()
newtype Handle = Handle {submit :: forall a. (Show a) => Program a -> IO (Either Error a)}
data Job where
    Job :: (Show a) => Program a -> (Either Error a -> IO ()) -> Job

runEventStream ::
    forall m.
    (MonadIO m) =>
    (Error -> m ()) ->
    (Text -> m ()) ->
    (forall a. Action a -> ExceptT Error m a) ->
    (Handle -> S.Stream IO [Event]) ->
    m ()
runEventStream handleError log' run' feeds = do
    jobs <- liftIO newChan
    let runnerHandle = Handle \prog -> do
            m <- newEmptyMVar
            writeChan jobs $ Job prog $ putMVar m
            takeMVar m
        runJob (Job prog f) = do
            r <-
                runM
                    . Freer.runError
                    . interpret
                        ( \(Log t) ->
                            sendM $ log' t
                        )
                    . interpret
                        ( \a -> do
                            logMsg $ showT a
                            either Freer.throwError pure =<< sendM (runExceptT $ run' a)
                        )
                    . raiseLast
                    $ prog
            either handleError (log' . showT) r
            liftIO $ f r
    S.fold
        ( SF.drainMapM \case
            Right prog -> runJob $ Job prog mempty
            Left job -> runJob job
        )
        . S.concatMap S.fromList
        . S.cons [Right $ logMsg "Starting..."]
        . S.morphInner liftIO
        $ S.parList
            id
            [ fmap (pure . Left) . S.repeatM $ readChan jobs
            , Right <<$>> feeds runnerHandle
            ]

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error

-- TODO what I really want is just to catch all non-async exceptions
-- is there no good way to do this? maybe by catching all then re-throwing asyncs?
-- it does seem to be difficult - https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell
-- TODO on the other hand, should the other exception types used here be made subtypes of `IOException`?
catchActionErrors :: forall m a. (MonadCatch m, MonadError Error m) => m a -> m a
catchActionErrors = catchMany @'[IOException] $ throwError . Error "Error when running action"

data Action a where
    Exit :: ExitCode -> Action ()
    PowerOff :: Action ()
    ResetError :: Action ()
    GetLightPower :: RoomLightPair c -> Action Bool
    SetLightPower :: RoomLightPair c -> Bool -> Action ()
    GetLightColour :: RoomLightPair c -> Action HSBK
    SetLightColour :: {light :: RoomLightPair FullColours, delay :: NominalDiffTime, colour :: HSBK} -> Action ()
    SetLightColourBK :: {lightBK :: RoomLightPair KelvinOnly, delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> Action () -- TODO we should in principle be allowed to reuse the name `light` for the field - https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
    SetDeskPower :: DeskPowerDevice -> Bool -> Action ()
    SendEmail :: {subject :: Text, body :: Text} -> Action ()
    SetOtherLED :: Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
deriving instance Show (Action a)
data Light (r :: Room) (c :: LightColours) where
    Lamp :: Light LivingRoom FullColours
    BedroomLight :: Light Bedroom KelvinOnly
    OfficeLight :: Light Office KelvinOnly
deriving instance Show (Light r c)
type data LightColours = FullColours | KelvinOnly
type data Room
    = LivingRoom
    | Bedroom
    | Office
data DeskPowerDevice
    = Computer
    | MainMonitor
    | PortraitMonitor
    | -- | Turning these off is a really bad idea, since they power Clark itself!
      UsbPorts
    deriving (Show, Read)

-- TODO can we use singletons for this?
data SRoom (r :: Room) where
    SLivingRoom :: SRoom LivingRoom
    SBedroom :: SRoom Bedroom
    SOffice :: SRoom Office
deriving instance Show (SRoom r)

-- NB this also serves as a handy way to assert that `RoomConstraints` holds for all rooms
enumerateRooms :: [Exists RoomConstraints SRoom]
enumerateRooms =
    [ Exists SLivingRoom
    , Exists SBedroom
    , Exists SOffice
    ]

enumerateLights :: SRoom r -> [Exists' (Light r)]
enumerateLights = \case
    SLivingRoom -> [Exists' Lamp]
    SBedroom -> [Exists' BedroomLight]
    SOffice -> [Exists' OfficeLight]

enumerateRoomLights :: [Exists' RoomLightPair]
enumerateRoomLights =
    concatMap
        (\(Exists r) -> map (\(Exists l) -> Exists $ RoomLightPair r l) $ enumerateLights r)
        enumerateRooms

-- TODO we can't use the type synonym directly without the unreleased `-XUnsaturatedTypeFamilies`
type RoomConstraints0 r =
    ( Typeable r
    , FromHttpApiData (SRoom r)
    , FromHttpApiData (Exists' (Light r))
    , FromHttpApiData (Light r KelvinOnly)
    , FromHttpApiData (Light r FullColours)
    )
class (RoomConstraints0 r) => RoomConstraints r
instance (RoomConstraints0 r) => RoomConstraints r

-- TODO use explicit type arguments once available (GHC 9.10?) to simplify this
forEachRoom :: (forall (r :: Room). (RoomConstraints r) => Proxy r -> x) -> [x]
forEachRoom f = enumerateRooms <&> \(Exists @_ @r _) -> f $ Proxy @r

-- | A dependent pair of a room and a light in that room.
data RoomLightPair c where
    RoomLightPair :: SRoom r -> Light r c -> RoomLightPair c

-- TODO separation is annoying - fix in Fourmolu
deriving instance Show (RoomLightPair c)

roomName :: SRoom r -> Text
roomName = \case
    SLivingRoom -> "Living Room"
    SBedroom -> "Bedroom"
    SOffice -> "Office"

lightName :: Light r c -> Text
lightName = \case
    Lamp -> "Lamp"
    BedroomLight -> "Ceiling"
    OfficeLight -> "Ceiling"

-- TODO is there a way to derive some of this?
-- if we could do `deriving instance Read (Light NoColour)` that might be a good start
instance FromHttpApiData (SRoom LivingRoom) where
    parseUrlPiece = \case
        "living-room" -> Right SLivingRoom
        s -> Left $ "unknown room name: " <> s
instance ToHttpApiData (SRoom LivingRoom) where
    toUrlPiece = \case
        SLivingRoom -> "living-room"
instance FromHttpApiData (SRoom Bedroom) where
    parseUrlPiece = \case
        "bedroom" -> Right SBedroom
        s -> Left $ "unknown room name: " <> s
instance ToHttpApiData (SRoom Bedroom) where
    toUrlPiece = \case
        SBedroom -> "bedroom"
instance FromHttpApiData (SRoom Office) where
    parseUrlPiece = \case
        "office" -> Right SOffice
        s -> Left $ "unknown room name: " <> s
instance ToHttpApiData (SRoom Office) where
    toUrlPiece = \case
        SOffice -> "office"
instance FromHttpApiData (Exists' (Light LivingRoom)) where
    parseUrlPiece = \case
        "lamp" -> Right $ Exists' Lamp
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Exists' (Light LivingRoom)) where
    toUrlPiece = \case
        Exists Lamp -> "lamp"
instance FromHttpApiData (Exists' (Light Bedroom)) where
    parseUrlPiece = \case
        "main" -> Right $ Exists' BedroomLight
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Exists' (Light Bedroom)) where
    toUrlPiece = \case
        Exists BedroomLight -> "main"
instance FromHttpApiData (Exists' (Light Office)) where
    parseUrlPiece = \case
        "main" -> Right $ Exists' OfficeLight
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Exists' (Light Office)) where
    toUrlPiece = \case
        Exists OfficeLight -> "main"
instance FromHttpApiData (Light LivingRoom FullColours) where
    parseUrlPiece = \case
        "lamp" -> Right Lamp
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light LivingRoom FullColours) where
    toUrlPiece = \case
        Lamp -> "lamp"
instance FromHttpApiData (Light LivingRoom KelvinOnly) where
    parseUrlPiece = \case
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light LivingRoom KelvinOnly) where
    toUrlPiece = \case {}
instance FromHttpApiData (Light Bedroom FullColours) where
    parseUrlPiece = \case
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light Bedroom FullColours) where
    toUrlPiece = \case {}
instance FromHttpApiData (Light Bedroom KelvinOnly) where
    parseUrlPiece = \case
        "main" -> Right BedroomLight
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light Bedroom KelvinOnly) where
    toUrlPiece = \case
        BedroomLight -> "main"
instance FromHttpApiData (Light Office FullColours) where
    parseUrlPiece = \case
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light Office FullColours) where
    toUrlPiece = \case {}
instance FromHttpApiData (Light Office KelvinOnly) where
    parseUrlPiece = \case
        "main" -> Right OfficeLight
        s -> Left $ "unknown light name: " <> s
instance ToHttpApiData (Light Office KelvinOnly) where
    toUrlPiece = \case
        OfficeLight -> "main"
instance
    ( FromHttpApiData (Light LivingRoom c)
    , FromHttpApiData (Light Bedroom c)
    , FromHttpApiData (Light Office c)
    ) =>
    FromHttpApiData (RoomLightPair c)
    where
    parseUrlPiece = \case
        (T.stripPrefix "living-room-" -> Just l) -> RoomLightPair SLivingRoom <$> parseUrlPiece l
        (T.stripPrefix "bedroom-" -> Just l) -> RoomLightPair SBedroom <$> parseUrlPiece l
        (T.stripPrefix "office-" -> Just l) -> RoomLightPair SOffice <$> parseUrlPiece l
        s -> Left $ "unknown room name: " <> s
instance
    ( ToHttpApiData (Light LivingRoom c)
    , ToHttpApiData (Light Bedroom c)
    , ToHttpApiData (Light Office c)
    ) =>
    ToHttpApiData (RoomLightPair c)
    where
    toUrlPiece = \case
        RoomLightPair SLivingRoom l -> "living-room-" <> toUrlPiece l
        RoomLightPair SBedroom l -> "bedroom-" <> toUrlPiece l
        RoomLightPair SOffice l -> "office-" <> toUrlPiece l
instance FromHttpApiData (Exists' RoomLightPair) where
    parseUrlPiece t =
        first T.unlines $
            firstRight
                [ Exists' <$> parseUrlPiece @(RoomLightPair FullColours) t
                , Exists' <$> parseUrlPiece @(RoomLightPair KelvinOnly) t
                ]
instance ToHttpApiData (Exists' RoomLightPair) where
    toUrlPiece = \case
        Exists l@(RoomLightPair SLivingRoom Lamp) -> toUrlPiece l
        Exists l@(RoomLightPair SBedroom BedroomLight) -> toUrlPiece l
        Exists l@(RoomLightPair SOffice OfficeLight) -> toUrlPiece l
instance HasForeignType Curl Examples (RoomLightPair FullColours) where
    typeFor =
        typeForExamples
            [ RoomLightPair SLivingRoom Lamp
            ]
instance HasForeignType Curl Examples (RoomLightPair KelvinOnly) where
    typeFor =
        typeForExamples
            [ RoomLightPair SBedroom BedroomLight
            , RoomLightPair SOffice OfficeLight
            ]
instance HasForeignType Curl Examples (Exists' RoomLightPair) where
    typeFor =
        typeForExamples
            [ Exists' $ RoomLightPair SLivingRoom Lamp
            , Exists' $ RoomLightPair SBedroom BedroomLight
            , Exists' $ RoomLightPair SOffice OfficeLight
            ]
instance FromHttpApiData DeskPowerDevice where
    parseUrlPiece = \case
        "computer" -> Right Computer
        "main-monitor" -> Right MainMonitor
        "portrait-monitor" -> Right PortraitMonitor
        "usb-ports" -> Right UsbPorts
        s -> Left $ "unknown desk device: " <> s
instance ToHttpApiData DeskPowerDevice where
    toUrlPiece = \case
        Computer -> "computer"
        MainMonitor -> "main-monitor"
        PortraitMonitor -> "portrait-monitor"
        UsbPorts -> "usb-ports"
instance HasForeignType Curl Examples DeskPowerDevice where
    typeFor = typeForExamples [MainMonitor, UsbPorts]

data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , ledOtherPin :: Int
    , emailPipe :: FilePath
    , sshTimeout :: Int
    , getLight :: forall c. RoomLightPair c -> Device
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
    SetOtherLED b -> setLED opts.ledOtherPin b
    SetSystemLEDs b -> writePipe opts.systemLedPipe . showT $ fromEnum b
  where
    showOutput out err = liftIO $ for_ [("stdout", out), ("stderr", err)] \(s, t) ->
        unless (B.null t) $ T.putStrLn ("    " <> s <> ": ") >> B.putStr (BC8.strip t)
    throwWhenFailureExitCode s ec =
        unless (ec == ExitSuccess) $ throwError $ Error s ec
    writePipe p t =
        liftIO (T.writeFile p t)
            `catchDNE` \_ ->
                throwError $ SimpleError "Pipe doesn't exist"
      where
        catchDNE = catchIf isDoesNotExistError

makeEffect ''Action

-- toggleLight :: (Member Action effs) => RoomLightPair c -> Eff effs ()
-- toggleLight :: RoomLightPair c -> Eff '[Action] ()
toggleLight :: RoomLightPair c -> CompoundAction ()
toggleLight l = setLightPower l . not =<< getLightPower l

-- sleepOrWake :: (Member Action effs) => NominalDiffTime -> Word16 -> Eff effs ()
-- sleepOrWake :: NominalDiffTime -> Word16 -> Eff '[Action] ()
sleepOrWake :: NominalDiffTime -> Word16 -> CompoundAction ()
sleepOrWake lifxMorningDelay lifxMorningKelvin =
    getLightPower light >>= \(not -> morning) -> do
        setSystemLEDs morning
        setLightPower light morning
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
  where
    light = RoomLightPair SBedroom BedroomLight
