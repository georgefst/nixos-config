{-# LANGUAGE LexicalNegation #-}

module George.Feed.WebServer (feed, Opts (..)) where

import George.Core

import API
import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Evdev.Codes qualified as Evdev
import Lifx.Lan hiding (SetLightPower)
import Network.HTTP.Types
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Streamly.Data.Stream.Prelude qualified as S
import Util.Servant.Streamly qualified as Servant

data Opts = Opts
    { port :: Warp.Port
    , curlDocsCallback :: Text -> IO ()
    , webRoot :: FilePath
    , doorbellSound :: FilePath
    }

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Servant.stream @(NamedRoutes API.Routes :<|> Raw)
            Servant.Opts
                { warpSettings = Warp.setPort opts.port Warp.defaultSettings
                , routes = \act ->
                    Routes
                        { exit = f noContent act $ send Exit
                        , resetError = f noContent act $ send ResetError
                        , powerOff = f noContent act $ send PowerOff
                        , reboot = f noContent act $ send Reboot
                        , pressKey = f noContent act . send . PressKey . toEvdevKey
                        , getBulbs = \rescan -> f id act $ when rescan (send LightReScan) >> send GetAllLights
                        , getBulbStatus = \(BulbGroup group) (BulbName name) -> f id act do
                            -- TODO repetitive...
                            dev <- send $ GetLightByGroupAndName group name
                            ls <- send $ GetLightState dev
                            pure BulbStatus{power = ls.power /= 0, colour = fromHSBK ls.hsbk}
                        , setBulbPower = \(BulbGroup group) (BulbName name) power -> f noContent act do
                            dev <- send $ GetLightByGroupAndName group name
                            send $ SetLightPower dev power
                        , setBulbColour = \(BulbGroup group) (BulbName name) colour -> f noContent act do
                            dev <- send $ GetLightByGroupAndName group name
                            send . SetLightColour False dev 0 $ toHSBK colour
                        , getSpotifyDevices = f id act $ send SpotifyGetDevices
                        , spotifyTransfer = f noContent act . (send . flip SpotifyTransfer True <=< send . SpotifyGetDevice)
                        , getHifiPower = f id act $ send GetHifiPlugPower
                        , setHifiPower = f noContent act . send . SetHifiPlugPower
                        , -- TODO I really don't like this string being duplicated - we need to rethink our IR types
                          toggleTvPower = f noContent act $ send $ SendIR IRTV "KEY_POWER"
                        , doorbell = f noContent act $ send $ PlayAudio opts.doorbellSound -- TODO add desktop notification etc.
                        , getSpdifVolume = f id act $ send GetSpdifVolume
                        , setSpdifVolume = f noContent act . send . SetSpdifVolume
                        , incrementSpdifVolume = f noContent act $ send . SetSpdifVolume . addPercentageClamped (percentageClamped 5) =<< send GetSpdifVolume
                        , decrementSpdifVolume = f noContent act $ send . SetSpdifVolume . flip subtractPercentageClamped (percentageClamped 5) =<< send GetSpdifVolume
                        , getSpdifMute = f id act $ send GetSpdifMute
                        , setSpdifMute = f noContent act . send . SetSpdifMute
                        , irSwitcher = f noContent act . send . SendIR IRSwitcher
                        }
                        -- TODO disable cache headers?
                        -- or is there some way we can force a full refresh on mobile Firefox?
                        -- could we force a cache clear only when the content (or NixOS hash) doesn't match?
                        -- TODO maybe `Raw` shouldn't just be a fallback
                        -- move the web app to a `/web` subroute or something to avoid collisions?
                        -- not a huge priority since static files are under the "assets" path anyway?
                        :<|> Tagged (staticApp $ defaultWebAppSettings opts.webRoot)
                }
            <&> \case
                Servant.Event x -> Just [x]
                Servant.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    noContent () = NoContent
    -- Enqueue the action, then block until the event loop tells us how it went.
    -- An `Error` has already been logged and lit the error LED by that point, but we still need to
    -- answer the HTTP request, so we turn it in to a 500 rather than leaving the client hanging.
    f show' (act :: Event -> IO ()) a = do
        r <- liftIO do
            m <- newEmptyMVar
            act $ ActionEvent (putMVar m) a
            takeMVar m
        either (\e -> throwError err500{errBody = BL.fromStrict . encodeUtf8 $ renderError e}) (pure . show') r

toEvdevKey :: API.Key -> Evdev.Key
toEvdevKey = \case
    API.KeyEnter -> Evdev.KeyEnter
    API.KeyUp -> Evdev.KeyUp
    API.KeyDown -> Evdev.KeyDown
    API.KeyLeft -> Evdev.KeyLeft
    API.KeyRight -> Evdev.KeyRight
    API.KeyEsc -> Evdev.KeyEsc
    API.KeySpace -> Evdev.KeySpace
    API.KeyTab -> Evdev.KeyTab
    API.KeySuper -> Evdev.KeyLeftmeta

toHSBK :: BulbColour -> HSBK
toHSBK BulbColour{kelvin = Kelvin kelvin, ..} = HSBK{..}
fromHSBK :: HSBK -> BulbColour
fromHSBK HSBK{..} = BulbColour{kelvin = Kelvin kelvin, ..}
