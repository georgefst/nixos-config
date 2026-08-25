{-# LANGUAGE DerivingVia #-}

-- TODO a lot of these types should go in some utils module or elsewhere
-- though that requires a bit of project dependency shuffling
module API where

import Control.Monad
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Bifunctor
import Data.Ord
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import GHC.Generics (Generic)
import Servant.API
import Text.Read

-- TODO more of these probably shouldn't really by `GET`s
-- TODO Mpris commands (Spotify play/pause), IR...
data Routes mode = Routes
    { exit :: mode :- "exit" :> PutNoContent
    , resetError :: mode :- "reset-error" :> PutNoContent
    , powerOff :: mode :- "poweroff" :> PutNoContent
    , reboot :: mode :- "reboot" :> PutNoContent
    , pressKey :: mode :- Capture "key" Key :> PutNoContent
    , getBulbs :: mode :- "lights" :> Capture "rescan" Bool :> Get '[JSON] [BulbInfo]
    , getBulbStatus :: mode :- "light" :> Capture "group" BulbGroup :> Capture "light" BulbName :> Get '[JSON] BulbStatus
    , setBulbPower :: mode :- "light" :> Capture "group" BulbGroup :> Capture "bulb" BulbName :> "power" :> Capture "power" Bool :> PutNoContent
    , setBulbColour :: mode :- "light" :> Capture "group" BulbGroup :> Capture "light" BulbName :> "colour" :> ReqBody '[JSON] BulbColour :> PutNoContent
    , getSpotifyDevices :: mode :- "spotify" :> "devices" :> Get '[JSON] [SpotifyDevice]
    , spotifyTransfer :: mode :- "spotify" :> Capture "device" Text :> PutNoContent
    , getHifiPower :: mode :- "hifi" :> Get '[JSON] Bool
    , setHifiPower :: mode :- "hifi" :> Capture "power" Bool :> PutNoContent
    , toggleTvPower :: mode :- "tv" :> PutNoContent
    , doorbell :: mode :- "doorbell" :> GetNoContent -- has to be a GET due to Shelly button limitations
    , getSpdifVolume :: mode :- "spdif" :> "volume" :> Get '[JSON] Percentage
    , setSpdifVolume :: mode :- "spdif" :> "volume" :> Capture "percent" Percentage :> PutNoContent
    , incrementSpdifVolume :: mode :- "spdif" :> "up" :> PutNoContent
    , decrementSpdifVolume :: mode :- "spdif" :> "down" :> PutNoContent
    , getSpdifMute :: mode :- "spdif" :> "mute" :> Get '[JSON] Bool
    , setSpdifMute :: mode :- "spdif" :> "mute" :> Capture "muted" Bool :> PutNoContent
    , irSwitcher :: mode :- "ir" :> "switcher" :> Capture "key" Text :> PutNoContent
    }
    deriving (Generic)

data Key
    = KeyEnter
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyEsc
    | KeySpace
    | KeyTab
    | KeySuper
    deriving stock (Show, Read, Bounded, Enum)
    deriving (ToHttpApiData) via HttpShow Key
    deriving (FromHttpApiData) via HttpShow Key

newtype BulbName = BulbName Text
    deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToHttpApiData, FromHttpApiData)
newtype BulbGroup = BulbGroup Text
    deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString, ToHttpApiData, FromHttpApiData)
data BulbInfo = BulbInfo
    { name :: BulbName
    , group :: BulbGroup
    , hasKelvin :: Bool
    , hasColour :: Bool
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
data BulbStatus = BulbStatus
    { power :: Bool
    , colour :: BulbColour
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data BulbColour = BulbColour
    { hue :: Word16
    , saturation :: Word16
    , brightness :: Word16
    , kelvin :: Kelvin
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- TODO we already do something similar in Clark, and talked about upstreaming...
newtype Kelvin = Kelvin Word16
    deriving newtype (Eq, Ord, Show, Read, Enum, Num, Real, Integral, ToJSON, FromJSON)
instance Bounded Kelvin where
    minBound = Kelvin 1500
    maxBound = Kelvin 9000

data SpotifyDevice = SpotifyDevice
    { name :: Text
    , -- TODO given there'll only be one of these active, I'd prefer not to store in every record
      -- actually, idk maybe it is just easier
      isActive :: Bool
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype HttpShow a = HttpShow a
instance (Show a) => ToHttpApiData (HttpShow a) where
    toUrlPiece = T.pack . show . \(HttpShow x) -> x
instance (Read a) => FromHttpApiData (HttpShow a) where
    parseUrlPiece = bimap T.pack HttpShow . readEither <=< parseUrlPiece

-- TODO we could consider adding '%' for `Read`/`Show`, but then that would break `HttpShow`
newtype Percentage = Percentage Word8
    deriving newtype (Eq, Ord, Show)
    deriving (ToHttpApiData, FromHttpApiData) via HttpShow Percentage
instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 100
instance Read Percentage where
    readPrec = maybe (fail "out of range") pure . percentage =<< readPrec
instance ToJSON Percentage where
    toJSON (Percentage p) = toJSON p
instance FromJSON Percentage where
    parseJSON v = maybe (fail "out of range") pure . percentage =<< parseJSON v
percentage :: Word8 -> Maybe Percentage
percentage n = Percentage (fromIntegral n) <$ guard (n >= 0 && n <= 100)
percentageClamped :: Word8 -> Percentage
percentageClamped = Percentage . fromIntegral . clamp (0, 100)
fromPercentage :: (Num a) => Percentage -> a
fromPercentage (Percentage n) = fromIntegral n
addPercentageClamped :: Percentage -> Percentage -> Percentage
addPercentageClamped x y = percentageClamped $ fromPercentage x + fromPercentage y
subtractPercentageClamped :: Percentage -> Percentage -> Percentage
subtractPercentageClamped x y = percentageClamped $ fromPercentage x - fromPercentage y

-- TODO ideally this would be a thinner wrapper around a bounded integer type, but we can't
-- newtype Percentage = Percentage (Finite 101) -- or `Data.Finite.Integral.Finite Word8 101`
--     deriving newtype (Eq, Ord)
--     deriving (ToHttpApiData, FromHttpApiData) via HttpShow Percentage
-- instance Show Percentage where
--     show (Percentage p) = show $ toInteger p
-- instance Read Percentage where
--     readPrec = maybe (fail "out of range") pure . percentage =<< readPrec
-- instance ToJSON Percentage where
--     toJSON (Percentage p) = toJSON $ toInteger p
-- instance FromJSON Percentage where
--     parseJSON v = maybe (fail "out of range") pure . percentage =<< parseJSON v
-- percentage :: Int -> Maybe Percentage
-- percentage n = Percentage <$> packFinite (fromIntegral n)
-- percentageClamped :: Int -> Percentage
-- percentageClamped n = Percentage $ packFiniteClamped $ fromIntegral n
-- fromPercentage :: (Num a) => Percentage -> a
-- fromPercentage (Percentage n) = fromInteger $ toInteger n
-- addPercentageClamped :: Percentage -> Percentage -> Percentage
-- addPercentageClamped (Percentage x) (Percentage y) = Percentage $ clampFinite $ Finite.add x y
-- subtractPercentageClamped :: Percentage -> Percentage -> Percentage
-- subtractPercentageClamped (Percentage x) (Percentage y) =
--     Percentage
--         . either (const minBound) clampFinite
--         $ Finite.sub x y
-- -- TODO https://github.com/mniip/finite-typelits/issues/32
-- packFiniteClamped :: (KnownNat n) => Integer -> Finite n
-- packFiniteClamped x = r
--   where
--     r = finite $ clamp (toInteger l, toInteger u) x
--     l = minBound `asTypeOf` r
--     u = maxBound `asTypeOf` r
-- clampFinite :: (KnownNat m) => Finite n -> Finite m
-- clampFinite = packFiniteClamped . getFinite
