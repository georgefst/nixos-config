{-# LANGUAGE DerivingVia #-}

module API where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor
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

-- TODO move to some utility module?
newtype HttpShow a = HttpShow a
instance (Show a) => ToHttpApiData (HttpShow a) where
    toUrlPiece = T.pack . show . \(HttpShow x) -> x
instance (Read a) => FromHttpApiData (HttpShow a) where
    parseUrlPiece = bimap T.pack HttpShow . readEither <=< parseUrlPiece
