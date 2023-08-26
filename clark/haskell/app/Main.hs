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
import Lifx.Lan hiding (SetColor, SetLightPower)
import Lifx.Lan qualified as Lifx
import MQTT.Meross qualified
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi
import Options.Generic
import RawFilePath
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream qualified as S
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream.StreamK qualified as SK (hoist)
import System.Directory qualified as Dir
import System.Exit
import System.IO
import Text.Pretty.Simple
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

data Opts = Opts
    { gpioChip :: B.ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , ceilingLightName :: Text
    , lampName :: Text
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
    eventMVar <- newEmptyMVar

    let
        setLED :: (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( gets (Map.lookup pin) >>= \case
                    Just h -> GPIO.reset h >> modify (Map.delete pin)
                    Nothing -> log' "LED is already off"
                )
                ( gets (Map.lookup pin) >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= modify . Map.insert pin
                    Just _ -> log' "LED is already on"
                )
          where
            log' = liftIO . putMVar eventMVar . LogEvent

        handleError :: (MonadIO m, MonadState AppState m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

    -- TODO initialisation stuff - encapsulate this better somehow, without it being killed by LIFX failure
    -- TODO even discounting LIFX issue, unclear how to do this in Streamly 0.9, as there's no `Monad (Stream IO)`
    eventSocket <- socket AF_INET Datagram defaultProtocol
    bind eventSocket $ SockAddrInet (fromIntegral opts.receivePort) 0
    let gpioMonitor =
            GPIO.mon opts.gpioChip (putMVar eventMVar . LogEvent) opts.buttonDebounce opts.buttonPin
                . putMVar eventMVar
                $ ActionEvent (simpleAction ResetError)

    race_
        ( gpioMonitor
            `race_` Warp.runSettings
                (warpSettings opts.httpPort $ putMVar eventMVar . ErrorEvent . Error "HTTP error")
                (webServer $ liftIO . putMVar eventMVar . ActionEvent)
        )
        . flip evalStateT mempty
        . flip runLoggingT (liftIO . T.putStrLn)
        . runLifxUntilSuccess
            (either (handleError . Error "Light not found") (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
        $ do
            (ceilingLight, lamp) <- do
                ds <-
                    traverse (\d -> (d,) <$> sendMessage d GetColor)
                        =<< discoverDevices Nothing
                let f name = maybe (throwError name) (pure . fst) $ find ((== name) . (.label) . snd) ds
                (,) <$> f opts.ceilingLightName <*> f opts.lampName
            S.fold
                ( SF.drainMapM \case
                    ErrorEvent e -> handleError e
                    LogEvent t -> logMessage t
                    ActionEvent action ->
                        (either handleError pure <=< runExceptT)
                            . (logMessage . snd @() <=< runM)
                            . subsumeFront
                            . translate (runSimpleAction (opts & \Opts{..} -> SimpleActionOpts{..}))
                            . Eff.runWriter
                            $ do
                                Eff.tell case action of
                                    SimpleAction a _ -> showT a
                                    ToggleCeilingLight -> "ToggleCeilingLight"
                                    SleepOrWake -> "SleepOrWake"
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
    GetLightPower :: Light a -> SimpleAction Bool
    SetLightPower :: Light a -> Bool -> SimpleAction ()
    GetLightColour :: Light a -> SimpleAction HSBK
    SetLightColour :: {light :: Light FullColours, delay :: NominalDiffTime, colour :: HSBK} -> SimpleAction ()
    SetLightColourBK :: {lightBK :: Light KelvinOnly, delay :: NominalDiffTime, brightness :: Word16, kelvin :: Word16} -> SimpleAction () -- TODO we should in principle be allowed to reuse the name `light` for the field - https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
    SetDeskUSBPower :: Bool -> SimpleAction ()
    SendEmail :: {subject :: Text, body :: Text} -> SimpleAction ()
    SuspendLaptop :: SimpleAction ()
    SetOtherLED :: Bool -> SimpleAction ()
    SetSystemLEDs :: Bool -> SimpleAction ()
    RootCommand :: Text -> SimpleAction ()
deriving instance Show (SimpleAction a)
data SimpleActionOpts = SimpleActionOpts
    { ledErrorPin :: Int
    , ledOtherPin :: Int
    , emailPipe :: FilePath
    , sshTimeout :: Int
    , ceilingLight :: Device
    , lamp :: Device
    , laptopHostName :: Text
    , deskUsbPort :: Int
    , systemLedPipe :: FilePath
    , rootCmdPipe :: FilePath
    , setLED :: forall m. (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
    }

runSimpleAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadError Error m) => SimpleActionOpts -> SimpleAction a -> m a
runSimpleAction opts@SimpleActionOpts{setLED {- TODO GHC doesn't yet support impredicative fields -}} = \case
    ResetError -> setLED opts.ledErrorPin False
    GetLightPower l -> statePowerToBool <$> sendMessage (lightOpt l opts) GetPower
    SetLightPower l p -> sendMessage (lightOpt l opts) $ SetPower p
    GetLightColour l -> (.hsbk) <$> sendMessage (lightOpt l opts) Lifx.GetColor
    SetLightColour{..} -> sendMessage (lightOpt light opts) $ Lifx.SetColor colour delay
    SetLightColourBK{lightBK = light, ..} -> sendMessage (lightOpt light opts) $ Lifx.SetColor HSBK{..} delay
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
    SimpleAction :: SimpleAction a -> (a -> IO ()) -> Action
    ToggleCeilingLight :: Action
    SleepOrWake :: Action
simpleAction :: SimpleAction a -> Action
simpleAction = flip SimpleAction mempty
data ActionOpts = ActionOpts
    { lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    }
runAction :: (MonadIO m) => ActionOpts -> Action -> Eff '[SimpleAction, m] ()
runAction opts = \case
    SimpleAction a f -> liftIO . f =<< send a
    ToggleCeilingLight -> send . SetLightPower Ceiling . not =<< send (GetLightPower Ceiling)
    SleepOrWake ->
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
                        , delay = fromIntegral opts.lifxMorningSeconds
                        , brightness = maxBound
                        , kelvin = opts.lifxMorningKelvin
                        }
            send $ SetDeskUSBPower morning
            when night . void $ send SuspendLaptop
      where
        light = Ceiling

-- TODO re-evaluate this now that we have web API
decodeAction :: BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) Action
decodeAction =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ simpleAction ResetError
            1 -> pure ToggleCeilingLight
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ simpleAction SendEmail{..}
            3 -> pure $ simpleAction SuspendLaptop
            4 -> simpleAction . SetDeskUSBPower <$> B.get @Bool
            5 -> simpleAction <$> (SetLightColourBK Ceiling . secondsToNominalDiffTime <$> B.get <*> B.get <*> B.get)
            6 -> pure SleepOrWake
            7 -> simpleAction . SetLightPower Ceiling <$> B.get @Bool
            8 -> simpleAction . SetSystemLEDs <$> B.get @Bool
            n -> fail $ "unknown action: " <> show n

webServer :: (forall m. (MonadIO m) => Action -> m ()) -> Wai.Application
webServer f =
    makeOkapiApp id $
        asum
            [ withGetRoute "reset-error" $ f2 $ simpleAction ResetError
            , withGetRoute "get-light-power" do
                Exists l <- segParam
                f1 $ SimpleAction $ GetLightPower l
            , withGetRoute "set-light-power" do
                Exists l <- segParam
                p <- segParam
                f2 . simpleAction $ SetLightPower l p
            , withGetRoute "get-light-colour" do
                Exists light <- segParam
                f1 $ SimpleAction $ GetLightColour light
            , withGetRoute "set-light-colour" do
                Exists light <- segParam
                delay <- segParam @NominalDiffTime -- TODO why do we need this type app?
                case light of
                    Lamp -> do
                        hue <- segParam
                        saturation <- segParam
                        brightness <- segParam
                        kelvin <- segParam
                        f2 $ simpleAction SetLightColour{colour = HSBK{..}, ..}
                    Ceiling -> do
                        brightness <- segParam
                        kelvin <- segParam
                        f2 $ simpleAction SetLightColourBK{lightBK = light, ..}
            , withGetRoute "set-desk-usb-power" $ f2 . simpleAction . SetDeskUSBPower =<< segParam
            , withGetRoute "send-email" do
                subject <- segParam
                body <- segParam
                f2 $ simpleAction SendEmail{..}
            , withGetRoute "suspend-laptop" $ f2 $ simpleAction SuspendLaptop
            , withGetRoute "set-other-led" $ f2 . simpleAction . SetOtherLED =<< segParam
            , withGetRoute "set-system-leds" $ f2 . simpleAction . SetSystemLEDs =<< segParam
            , withGetRoute "toggle-ceiling-light" $ f2 ToggleCeilingLight
            , withGetRoute "sleep-or-wake" $ f2 SleepOrWake
            ]
  where
    withGetRoute s x = Okapi.get >> seg s >> x
    f1 :: (Show a) => ((a -> IO ()) -> Action) -> OkapiT IO Result
    f1 x = do
        m <- liftIO newEmptyMVar
        f $ x $ putMVar m
        okPlainText [] . (<> "\n") . TL.toStrict . pShowNoColor =<< liftIO (takeMVar m)
    f2 x = f x >> noContent []

warpSettings ::
    Warp.Port ->
    (forall a. (Show a) => a -> IO ()) ->
    Warp.Settings
warpSettings port logError =
    Warp.setLogger (curry3 $ unless . statusIsSuccessful . snd3 <*> logError)
        . Warp.setPort port
        $ Warp.defaultSettings

data Event
    = ActionEvent Action
    | LogEvent Text
    | ErrorEvent Error

data Light (c :: LightColours) where
    Ceiling :: Light KelvinOnly
    Lamp :: Light FullColours
deriving instance Show (Light c)
data LightColours = FullColours | KelvinOnly -- TODO use `type data` when available (GHC 9.6)
lightOpt :: Light a -> SimpleActionOpts -> Device
lightOpt = \case
    Ceiling -> (.ceilingLight)
    Lamp -> (.lamp)

-- TODO is there a way to derive some of this?
-- if we could do `deriving instance Read (Light NoColour)` that might be a good start
instance FromHttpApiData (Exists Light) where
    parseUrlPiece = \case
        "ceiling" -> Right $ Exists Ceiling
        "lamp" -> Right $ Exists Lamp
        s -> Left $ "unknown light name: " <> s
