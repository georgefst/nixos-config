{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OrPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import API
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Char
import Data.Coerce
import Data.Functor
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word
import Debug.Pretty.Simple (pTraceShow, pTraceShowId)
import Debug.Trace
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Text.Read (readMaybe)

data AppState = AppState
    { hifiPower :: Bool
    -- TODO maybe store a `Bulb -> _` function?
    -- oh, but Miso probably needs `Eq` for diffing
    }

-- TODO string conversion functions assume rooms must be a single word, i.e. no caps after the start
data Room
    = Bedroom
    | Kitchen
    | Garden
    deriving (Eq, Ord, Read, Show, Enum, Bounded)
data Bulb
    = BigLamp
    | SmallLamp
    | BedroomCeiling
    | Counter
    | KitchenCeiling
    | Deck
    | Pond
    deriving (Eq, Ord, Read, Show, Enum, Bounded)
bulbName :: Bulb -> BulbName
roomName :: Room -> BulbGroup
(bulbName, roomName) =
    ( BulbName . \x ->
        let r = f x
         in T.strip . fromMaybe r . firstJust (flip T.stripPrefix r . coerce . roomName) $ enumerate @Room
    , BulbGroup . f
    )
  where
    f = T.stripStart . T.pack . concatMap (\c -> if isUpper c then [' ', c] else [c]) . show
fromBulbName :: BulbGroup -> BulbName -> Maybe Bulb
_fromRoomName :: BulbGroup -> Maybe Room
(fromBulbName, _fromRoomName) =
    ( \g n -> let ws = f1 n in f2 ws <|> f2 (T.unpack (coerce g) : ws)
    , f2 . f1
    )
  where
    f1 = map (T.unpack . T.toTitle) . T.words . coerce
    f2 = readMaybe . concat
bulbRoom :: Bulb -> Room
bulbRoom = \case
    BigLamp -> Bedroom
    SmallLamp -> Bedroom
    BedroomCeiling -> Bedroom
    Counter -> Kitchen
    KitchenCeiling -> Kitchen
    Deck -> Garden
    Pond -> Garden
bulbInfo :: BulbGroup -> BulbName -> Bulb -> BulbInfo
bulbInfo group name = \case
    BedroomCeiling; Pond -> BulbInfo{hasKelvin = True, hasColour = False, ..}
    Counter; Deck -> BulbInfo{hasKelvin = False, hasColour = False, ..}
    _ -> BulbInfo{hasKelvin = True, hasColour = True, ..}

initialState :: AppState
initialState =
    AppState
        { hifiPower = False
        }

corsPolicy :: CorsResourcePolicy
corsPolicy =
    simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = simpleMethods <> [methodPut]
        , corsRequestHeaders = [hContentType]
        }

main :: IO ()
main = do
    state <- newTVarIO initialState
    runSettings (setPort 8000 defaultSettings)
        . cors (const $ Just corsPolicy)
        $ serve
            (Proxy @(NamedRoutes Routes))
            Routes
                { exit = f "exit" [] NoContent
                , resetError = f "resetError" [] NoContent
                , powerOff = f "powerOff" [] NoContent
                , reboot = f "reboot" [] NoContent
                , pressKey = \x -> f "pressKey" [T.show x] NoContent
                , getBulbs = \rescan ->
                    f "getBulbs" [T.show rescan] $
                        enumerate <&> \b -> bulbInfo (roomName $ bulbRoom b) (bulbName b) b
                , getBulbStatus = \g b -> do
                    f1 "getBulbStatus" [coerce g, coerce b]
                    case fromBulbName g b of
                        Nothing -> do
                            let err = "unknown bulb"
                            liftIO $ putStrLn err
                            fail err
                        Just bulb -> do
                            f2 r
                            pure r
                          where
                            r =
                                BulbStatus
                                    { power = maybe False (odd . fromEnum . fst) . T.uncons $ coerce b
                                    , colour =
                                        let i = bulbInfo g b $ bulb
                                         in BulbColour
                                                { hue =
                                                    if i.hasColour
                                                        then
                                                            fromIntegral
                                                                . sum
                                                                . zipWith (*) (map (* 100) [1 ..])
                                                                . map ((- fromEnum 'a') . ord . toLower)
                                                                . filter isAlpha
                                                                . T.unpack
                                                                . T.reverse
                                                                $ coerce g <> coerce b
                                                        else 0
                                                , saturation = if i.hasColour then 40000 else 0
                                                , brightness = 30000
                                                , kelvin = if i.hasKelvin then 3500 else maxBound
                                                }
                                    }
                , setBulbPower = \g b power -> f "setBulbPower" [coerce g, coerce b, T.show power] NoContent
                , setBulbColour = \g b c -> f "setBulbColour" [coerce g, coerce b, T.show c] NoContent
                , getSpotifyDevices =
                    f
                        "getSpotifyDevices"
                        []
                        [ SpotifyDevice{name = "sol-spotify", isActive = True}
                        , SpotifyDevice{name = "phone", isActive = False}
                        , SpotifyDevice{name = "laptop", isActive = False}
                        ]
                , spotifyTransfer = \device -> f "spotifyTransfer" [device] NoContent
                , getHifiPower = do
                    p <- liftIO . readTVarIO $ state
                    f "getHifiPower" [] p.hifiPower
                , setHifiPower = \power -> do
                    liftIO . atomically $ modifyTVar' state (\s -> s{hifiPower = power})
                    f "setHifiPower" [T.show power] NoContent
                , toggleTvPower = f "toggleTvPower" [] NoContent
                }

-- TODO bit of a mess - we split this up when we realised that `getBulbStatus` needed to be able to fail
f :: (MonadIO m, Show a) => T.Text -> [T.Text] -> a -> m a
f n as r = f1 n as >> f2 r >> pure r
f1 :: (MonadIO m) => T.Text -> [T.Text] -> m ()
f1 n as = liftIO $ T.putStrLn (T.unwords (n : as))
f2 :: (MonadIO m, Show a) => a -> m ()
f2 = liftIO . print
