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
import Util.Lifx

import API hiding (Key (..))
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.State.Strict
import Data.Aeson.Optics
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (isSpace, toLower)
import Data.Foldable
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Maybe
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Evdev (EventData (KeyEvent), KeyEvent (..))
import Evdev.Codes (Key (..))
import Evdev.Uinput qualified as Uinput
import GHC.Records (HasField)
import Lifx.Lan (HSBK, MonadLifx)
import Lifx.Lan qualified as Lifx
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Optics
import Optics.State.Operators
import Options.Generic
import Spotify (MonadSpotify (throwClientError))
import Spotify qualified
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.Posix
import System.Process.Extra
import Util.GPIO.Persistent qualified as GPIO
import Util.Util

data AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    , bulbs :: Stream.Stream (Lifx.Device, Lifx.LightState, Lifx.StateGroup, Lifx.Product)
    , httpConnectionManager :: Manager
    , keySendSocket :: Socket
    , lightColourCache :: Maybe HSBK
    , -- TODO this should be reader rather than state
      uinput :: Uinput.Device
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
    S.Stream IO [Event] ->
    m ()
runEventStream handleError' log' run' =
    S.fold
        ( SF.drainMapM \case
            ErrorEvent e -> handleError' e
            LogEvent t -> log' t
            ActionEvent f action -> (either handleError' pure <=< runExceptT) $ runM do
                r <-
                    action & translate \a -> do
                        lift . log' $ showT a
                        run' a
                sendM . lift . log' $ showT r
                sendM . liftIO $ f r
        )
        . S.morphInner liftIO
        . S.concatMap S.fromList
        . S.cons [LogEvent "Starting..."]

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error
deriving instance Show Error

-- TODO what I really want is just to catch all non-async exceptions
-- is there no good way to do this? maybe by catching all then re-throwing asyncs?
-- it does seem to be difficult - https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell
-- TODO on the other hand, should the other exception types used here be made subtypes of `IOException`?
-- note that `LifxError` here is what keeps a flaky bulb from killing the whole event loop:
-- `lifx-lan` will already have retried, so by this point we just want to log it and carry on
catchActionErrors :: forall m a. (MonadCatch m, MonadError Error m) => m a -> m a
catchActionErrors = r $ throwClientError @IO
  where
    -- TODO this is just a cute/ugly trick to make up for the fact that Spotify library throws an unexported error type
    r :: forall x y. (Exception x) => (x -> y) -> m a -> m a
    r _ = catchMany @[IOException, HttpException, Lifx.LifxError, x] $ throwError . Error "Error when running action"

type CompoundAction a = Eff '[Action] a
data Action a where
    Exit :: Action ()
    ResetError :: Action ()
    Sleep :: NominalDiffTime -> Action ()
    PowerOff :: Action ()
    Reboot :: Action ()
    SetLED :: Int -> Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
    LaunchProgram :: FilePath -> Action ProcessID
    PressKey :: Key -> Action ()
    SendKey :: Key -> KeyEvent -> Action ()
    GetCurrentLight :: Action Lifx.Device
    GetCurrentLightGroup :: Action ByteString
    LightReScan :: Action ()
    NextLight :: Action ()
    GetLightPower :: Lifx.Device -> Action Bool
    SetLightPower :: Lifx.Device -> Bool -> Action ()
    UnsetLightColourCache :: Action ()
    GetLightColour :: Bool -> Lifx.Device -> Action HSBK
    SetLightColour :: Bool -> Lifx.Device -> NominalDiffTime -> HSBK -> Action ()
    GetLightState :: Lifx.Device -> Action Lifx.LightState
    GetLightName :: Lifx.Device -> Action Text
    GetLightsInGroup :: ByteString -> Action [Lifx.Device]
    Mpris :: Text -> Action ()
    SendIR :: IRDev -> Text -> Action ()
    GetHifiPlugPower :: Action Bool
    SetHifiPlugPower :: Bool -> Action ()
    ToggleHifiPlug :: Action () -- TODO why isn't this just a compound action?
    GetAllLights :: Action [BulbInfo]
    GetLightByGroupAndName :: Text -> Text -> Action Lifx.Device
    SpotifyGetDevices :: Action [SpotifyDevice]
    SpotifyGetDevice :: Text -> Action Spotify.DeviceID
    SpotifyTransfer :: Spotify.DeviceID -> Bool -> Action ()
    SpotifySearchAndPlay :: Spotify.SearchType -> Text -> Spotify.DeviceID -> Action ()
deriving instance Show (Action a)
data IRDev
    = IRTV -- TODO move to separate module to avoid need for prefixes?
    | IRSwitcher
    | IRFan
    deriving (Show)

data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , setLED :: forall m. (MonadState AppState m, MonadLog Text m, MonadIO m) => Int -> Bool -> m ()
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    , lifxIgnore :: [Text]
    , hifiPlugIp :: IP
    , irConfigDir :: Text
    }

runAction ::
    forall m a.
    (MonadIO m, MonadCatch m, MonadState AppState m, MonadLifx m, MonadLog Text m, MonadError Error m) =>
    ActionOpts ->
    Action a ->
    m a
runAction opts@ActionOpts{setLED {- TODO GHC doesn't yet support impredicative fields -}} = (.) catchActionErrors \case
    Exit -> liftIO exitSuccess
    ResetError -> setLED opts.ledErrorPin False
    Sleep t -> liftIO $ threadDelay' t
    PowerOff -> liftIO $ callProcess "sudo" ["poweroff"]
    Reboot -> liftIO $ callProcess "sudo" ["reboot"]
    SetLED n b -> setLED n b
    SetSystemLEDs b ->
        traverse_
            (\(l, v) -> liftIO $ readProcess "sudo" ["tee", "/sys/class/leds/" <> l <> "/trigger"] (v <> "\n"))
            (if b then [("ACT", "mmc0"), ("PWR", "default-on")] else [("ACT", "none"), ("PWR", "none")])
    LaunchProgram p -> liftIO $ forkProcess $ executeFile p True [] Nothing
    PressKey k -> do
        d <- use #uinput
        liftIO $ Uinput.writeBatch d [KeyEvent k Pressed, KeyEvent k Released]
    SendKey k e -> do
        -- TODO DRY this with my `net-evdev` repo
        sock <- use #keySendSocket
        liftIO . for_ opts.keySendIps $
            void
                . sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum e])
                . (SockAddrInet opts.keySendPort . (.unIP))
    GetCurrentLight -> (\(d, _, _, _) -> d) . Stream.head <$> use #bulbs
    GetCurrentLightGroup -> (\(_, _, g, _) -> g.group) . Stream.head <$> use #bulbs
    LightReScan ->
        maybe
            (logMessage "No valid LIFX devices found during re-scan - retaining old list")
            (\ds -> #bulbs .= Stream.cycle ds)
            . nonEmpty
            -- TODO these lines are duplicated with startup code
            -- but then it's probably all changing soon anyway
            =<< filterM
                ( \(_, Lifx.LightState{label}, _, _) ->
                    let good = label `notElem` opts.lifxIgnore
                     in logMessage ("LIFX device " <> bool "ignored" "found" good <> ": " <> label) >> pure good
                )
            =<< discoverLifx
    NextLight -> #bulbs %= Stream.tail
    GetLightPower l -> statePowerToBool <$> Lifx.sendMessage l Lifx.GetPower
    SetLightPower l p -> Lifx.sendMessage l $ Lifx.SetPower p
    UnsetLightColourCache -> #lightColourCache .= Nothing
    GetLightColour useCache l ->
        if useCache
            then maybe (throwError $ SimpleError "Light colour cache is empty") pure =<< use #lightColourCache
            else (.hsbk) <$> Lifx.sendMessage l Lifx.GetColor
    SetLightColour setCache l d c -> do
        when setCache $ #lightColourCache ?= c
        Lifx.sendMessage l $ Lifx.SetColor c d
    GetLightState l -> Lifx.sendMessage l Lifx.GetColor
    GetLightName l -> (.label) <$> Lifx.sendMessage l Lifx.GetColor
    GetLightsInGroup g -> do
        l Stream.:> ls <- use #bulbs
        pure $ (\(d, _, _, _) -> d) <$> filter (\(_, _, sg, _) -> sg.group == g) (l : Stream.takeWhile (/= l) ls)
    Mpris cmd -> do
        service <-
            maybe
                (throwError $ SimpleError "Failed to find spotifyd in qdbus output")
                pure
                . find ("rs.spotifyd" `isPrefixOf`)
                . map (dropWhile isSpace)
                . lines
                =<< liftIO (readProcess "qdbus" [] "")
        liftIO . void $
            readProcess
                "dbus-send"
                [ "--print-reply"
                , "--dest=" <> service
                , "/org/mpris/MediaPlayer2"
                , "org.mpris.MediaPlayer2.Player." <> T.unpack cmd
                ]
                ""
    SendIR dev cmd ->
        liftIO . void $
            readProcess
                "ir-ctl"
                ( map
                    T.unpack
                    [ "-k"
                    , ((opts.irConfigDir <> "/") <>) $ (<> ".toml") case dev of
                        IRTV -> "tv"
                        IRSwitcher -> "switcher"
                        IRFan -> "fan"
                    , "-K"
                    , cmd
                    ]
                )
                ""
    GetHifiPlugPower -> do
        response <- messageHifiPlug "Switch.GetStatus" ""
        maybe (throwError $ Error "Key \"output\" not found in HiFi plug response" response) pure $ responseBody response ^? key "output" % _Bool
    SetHifiPlugPower b -> void $ messageHifiPlug "Switch.Set" $ "&on=" <> B8.pack (map toLower $ show b)
    ToggleHifiPlug -> void $ messageHifiPlug "Switch.Toggle" ""
    -- TODO ha, this is a total hack, but sort of impressive
    -- same for next pattern
    -- I guess storing lights as a stream has long outlived its purpose...
    GetAllLights -> do
        l Stream.:> ls <- use #bulbs
        pure $ toBulbInfo <$> (l : Stream.takeWhile (/= l) ls)
    GetLightByGroupAndName g b -> do
        l Stream.:> ls <- use #bulbs
        let allBulbs = l : Stream.takeWhile (/= l) ls
        case find (\(_, ls', g', _) -> ls'.label == b && g'.label == g) allBulbs of
            Just (d, _, _, _) -> pure d
            Nothing -> throwError $ Error "Light not found" (g, b)
    SpotifyGetDevices -> do
        -- TODO does filtering for `isActive` make much sense?
        ds <- liftIO Spotify.getAvailableDevices
        pure [SpotifyDevice{name = d.name, isActive = d.isActive} | d <- ds]
    SpotifyGetDevice t -> do
        ds <- liftIO Spotify.getAvailableDevices
        maybe (throwError $ Error "Spotify device not found" (t, ds)) (pure . (.id)) $ find ((== t) . (.name)) ds
    SpotifyTransfer d b -> do
        liftIO $ Spotify.transferPlayback [d] b
    SpotifySearchAndPlay searchType query device -> do
        searchResult <- liftIO $ Spotify.search query [searchType] Nothing Nothing Spotify.noPagingParams
        case searchType of
            Spotify.AlbumSearch -> do
                album <- getURI searchResult.albums
                play (Just album) Nothing
            Spotify.ArtistSearch -> do
                -- TODO shuffle my liked songs by artist instead of playing top ones globally?
                artist <- getURI searchResult.artists
                play (Just artist) Nothing
            Spotify.PlaylistSearch -> do
                playlist <- getURI searchResult.playlists
                play (Just playlist) Nothing
            Spotify.TrackSearch -> do
                track <- getURI searchResult.tracks
                play Nothing (Just [track])
            -- TODO improve library to support all search types properly
            t -> throwError $ Error "Unsupported Spotify search type" t
      where
        getURI :: (HasField "uri" x Spotify.URI) => Maybe (Spotify.Paging x) -> m Spotify.URI
        getURI =
            maybe (throwError $ Error "No Spotify entries" (searchType, query)) pure
                . (fmap (.uri) . listToMaybe . (.items) =<<)
        play context item =
            liftIO
                . Spotify.startPlayback (Just device) -- ID for this device
                $ Spotify.StartPlaybackOpts context item Nothing
  where
    -- TODO factor out a module as a prototype library
    -- create a function for each endpoint, with appropriate arguments and response handling
    messageHifiPlug endpoint args = do
        man <- use #httpConnectionManager
        let host = encodeUtf8 $ showT opts.hifiPlugIp
        response <- liftIO $ httpLbs defaultRequest{host, path = "/rpc/" <> endpoint, queryString = "?id=0" <> args} man
        logMessage $ "HTTP response status code from HiFi plug: " <> showT (statusCode $ responseStatus response)
        -- TODO something to do with MonoLocalBinds, but I'm not sure _exactly_ why this type app is necessary
        pure @m response
    toBulbInfo (_, ls, sg, prod) =
        BulbInfo
            { name = BulbName ls.label
            , group = BulbGroup sg.label
            , hasColour = prod.features.color
            , hasKelvin = case prod.features.temperatureRange of
                Just (lo, hi) -> lo /= hi
                Nothing -> False
            }
