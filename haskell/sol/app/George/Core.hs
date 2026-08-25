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
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (isSpace, toLower)
import Data.Foldable
import Data.Functor (($>))
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
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
import SigmaDSP qualified
import Spotify (MonadSpotify (throwClientError))
import Spotify qualified
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.FilePath
import System.Posix
import System.Process.Extra
import Util.GPIO.Persistent qualified as GPIO
import Util.Util

-- | Everything we know about one bulb, as of the last scan.
data BulbEntry = BulbEntry
    { device :: Lifx.Device
    , light :: Lifx.LightState
    , group :: Lifx.StateGroup
    , productInfo :: Lifx.Product
    }
    deriving (Show, Generic)

mkBulbEntry :: (Lifx.Device, Lifx.LightState, Lifx.StateGroup, Lifx.Product) -> BulbEntry
mkBulbEntry (d, l, g, p) = BulbEntry{device = d, light = l, group = g, productInfo = p}

-- | How we refer to a bulb in logs and errors - the same "group/name" the web app shows.
describeBulb :: BulbEntry -> Text
describeBulb b = b.group.label <> "/" <> b.light.label

{- | The bulbs we currently believe in, in a stable, human-meaningful order.

This is the order `NextLight` cycles through, so it wants to match how the lights are laid out in
the world, rather than e.g. IP address order.
-}
sortedBulbs :: Map Lifx.Device BulbEntry -> [BulbEntry]
sortedBulbs = sortOn (\b -> (b.group.label, b.light.label)) . Map.elems

data AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    , bulbs :: Map Lifx.Device BulbEntry
    -- ^ Bulbs are removed from here as soon as they fail to answer us (see `sendToLight`), so this
    -- shrinks over time and only ever grows again on a re-scan.
    , currentLight :: Maybe Lifx.Device
    -- ^ Which bulb the physical remote is pointed at. `Nothing` means "not yet chosen, or the
    -- chosen one has gone away" - in which case we fall back to the first of `sortedBulbs`.
    , httpConnectionManager :: Manager
    , keySendSocket :: Socket
    , lightColourCache :: Maybe HSBK
    , -- TODO this should be reader rather than state
      uinput :: Uinput.Device
    }
    deriving (Generic)

data Event where
    -- | The callback is always invoked exactly once, even when the action fails - otherwise
    -- anything waiting on it (see `George.Feed.WebServer`) would wait forever.
    ActionEvent :: (Show a) => (Either Error a -> IO ()) -> (CompoundAction a) -> Event
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
            ActionEvent f action -> do
                r <-
                    runExceptT . runM $
                        action & translate \a -> do
                            lift . log' $ showT a
                            run' a
                -- note that `f` is called on both branches: it's how a caller learns the action is
                -- over, so skipping it on failure leaves them hanging
                case r of
                    Left e -> liftIO (f $ Left e) >> handleError' e
                    Right x -> log' (showT x) >> liftIO (f $ Right x)
        )
        . S.morphInner liftIO
        . S.concatMap S.fromList
        . S.cons [LogEvent "Starting..."]

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error
deriving instance Show Error

-- | A one-line rendering, for showing to a human (e.g. in an HTTP response body).
renderError :: Error -> Text
renderError = \case
    Error{title, body} -> title <> ": " <> showT body
    SimpleError t -> t

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
    PlayAudio :: FilePath -> Action ()
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
    GetSpdifVolume :: Action Percentage
    SetSpdifVolume :: Percentage -> Action ()
    GetSpdifMute :: Action Bool
    SetSpdifMute :: Bool -> Action ()
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
    , dspDevice :: FilePath
    , spdifVolumeRegister :: SigmaDSP.Address
    , spdifMuteRegister :: SigmaDSP.Address
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
    PlayAudio p -> liftIO $ callProcess "pw-play" [p]
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
    GetCurrentLight -> (.device) <$> currentBulb
    GetCurrentLightGroup -> (\b -> b.group.group) <$> currentBulb
    LightReScan -> do
        ds <- map mkBulbEntry <$> discoverLifxExcept opts.lifxIgnore
        case ds of
            [] -> logMessage "No valid LIFX devices found during re-scan - retaining old list"
            _ -> do
                let m = Map.fromList $ map (\b -> (b.device, b)) ds
                #bulbs .= m
                -- keep pointing at the same bulb if it's still there, else start again from scratch
                #currentLight %= (>>= \d -> guard (Map.member d m) $> d)
    NextLight -> do
        bs <- map (.device) . sortedBulbs <$> use #bulbs
        cur <- use #currentLight
        case bs of
            [] -> throwError noBulbs
            b0 : _ -> #currentLight ?= maybe b0 (\i -> bs !! ((i + 1) `mod` length bs)) (flip elemIndex bs =<< cur)
    GetLightPower l -> statePowerToBool <$> sendToLight l Lifx.GetPower
    SetLightPower l p -> sendToLight l $ Lifx.SetPower p
    UnsetLightColourCache -> #lightColourCache .= Nothing
    GetLightColour useCache l ->
        if useCache
            then maybe (throwError $ SimpleError "Light colour cache is empty") pure =<< use #lightColourCache
            else (.hsbk) <$> sendToLight l Lifx.GetColor
    SetLightColour setCache l d c -> do
        when setCache $ #lightColourCache ?= c
        sendToLight l $ Lifx.SetColor c d
    GetLightState l -> sendToLight l Lifx.GetColor
    GetLightName l -> (.label) <$> sendToLight l Lifx.GetColor
    GetLightsInGroup g -> map (.device) . filter (\b -> b.group.group == g) . sortedBulbs <$> use #bulbs
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
                [ "-k"
                , (T.unpack opts.irConfigDir </>) $ (<> ".toml") case dev of
                    IRTV -> "tv"
                    IRSwitcher -> "switcher"
                    IRFan -> "fan"
                , "-K"
                , T.unpack cmd
                ]
                ""
    GetHifiPlugPower -> do
        response <- messageHifiPlug "Switch.GetStatus" ""
        maybe (throwError $ Error "Key \"output\" not found in HiFi plug response" response) pure $ responseBody response ^? key "output" % _Bool
    SetHifiPlugPower b -> void $ messageHifiPlug "Switch.Set" $ "&on=" <> B8.pack (map toLower $ show b)
    ToggleHifiPlug -> void $ messageHifiPlug "Switch.Toggle" ""
    GetSpdifVolume -> withDsp \fd ->
        -- upstream only clamps the endpoints in `amplification2percent`, leaving the rest to the
        -- caller - see the note on `SigmaDSP.amplificationToPercent`
        SigmaDSP.amplificationToPercent <$> SigmaDSP.readGain fd opts.spdifVolumeRegister
    SetSpdifVolume v -> withDsp \fd ->
        SigmaDSP.writeGain fd opts.spdifVolumeRegister $ SigmaDSP.percentToAmplification v
    GetSpdifMute -> withDsp \fd -> (== 0) <$> SigmaDSP.readInt fd opts.spdifMuteRegister
    SetSpdifMute b -> withDsp \fd -> SigmaDSP.writeInt fd opts.spdifMuteRegister if b then 0 else 1
    GetAllLights -> map toBulbInfo . sortedBulbs <$> use #bulbs
    GetLightByGroupAndName g b ->
        maybe (throwError $ Error "Light not found" (g, b)) (pure . (.device))
            . find (\e -> e.light.label == b && e.group.label == g)
            . sortedBulbs
            =<< use #bulbs
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
    noBulbs = SimpleError "No LIFX devices are known - try a re-scan"

    {- The DSP: see `SigmaDSP` for the wire protocol, and `modules/sol.nix` for where the register
    addresses come from and what they mean. `catchActionErrors` turns a missing or unreadable
    `/dev/spidev0.0` in to an ordinary logged action failure, which is what makes it safe for the
    x86_64 `vms.sol` build (no SPI device at all) to run this same binary.

    Note the mute register's polarity: it's `InputSelector.MuteSPDIF`, a SigmaStudio `MuteNoSlew`,
    whose parameter is a pass-through flag, so 1 is *un*muted. (Don't generalise that to other mute
    registers in this profile: the post-mixer `Mute.Mute` is a `Switch` and runs the other way up.)
    -}
    withDsp :: (Fd -> IO x) -> m x
    withDsp = liftIO . SigmaDSP.withDevice opts.dspDevice

    {- Send a message to a bulb, and drop the bulb if it doesn't answer.

    `lifx-lan` has already retried by the time an error gets here, so a failure means the bulb has
    been unresponsive for several times the round trip time, and we're better off forgetting about
    it than making every future request wait for it too. It comes back on the next re-scan.

    Note that this can't fire for the `Set*` messages, which are fire-and-forget and so never fail
    - only a subsequent `Get` will notice that a bulb has gone. We still route them through here so
    that this keeps working if that ever changes. -}
    sendToLight :: Lifx.Device -> Lifx.Message r -> m r
    sendToLight d msg =
        Lifx.sendMessage d msg `catch` \(e :: Lifx.LifxError) -> do
            name <- maybe (showT d) describeBulb . Map.lookup d <$> use #bulbs
            #bulbs %= Map.delete d
            #currentLight %= (>>= \c -> guard (c /= d) $> c)
            throwError . Error "LIFX device unreachable, so dropping it until the next re-scan" $
                (name, displayException e)

    -- the bulb the physical remote is currently pointed at, defaulting to the first one
    currentBulb :: m BulbEntry
    currentBulb = do
        bs <- use #bulbs
        use #currentLight >>= \case
            Just d | Just b <- Map.lookup d bs -> pure b
            _ -> case sortedBulbs bs of
                [] -> throwError noBulbs
                b : _ -> #currentLight ?= b.device >> pure b

    -- TODO factor out a module as a prototype library
    -- create a function for each endpoint, with appropriate arguments and response handling
    messageHifiPlug endpoint args = do
        man <- use #httpConnectionManager
        let host = encodeUtf8 $ showT opts.hifiPlugIp
        response <- liftIO $ httpLbs defaultRequest{host, path = "/rpc/" <> endpoint, queryString = "?id=0" <> args} man
        logMessage $ "HTTP response status code from HiFi plug: " <> showT (statusCode $ responseStatus response)
        -- TODO something to do with MonoLocalBinds, but I'm not sure _exactly_ why this type app is necessary
        pure @m response
    toBulbInfo b =
        BulbInfo
            { name = BulbName b.light.label
            , group = BulbGroup b.group.label
            , hasColour = b.productInfo.features.color
            , hasKelvin = case b.productInfo.features.temperatureRange of
                Just (lo, hi) -> lo /= hi
                Nothing -> False
            }
