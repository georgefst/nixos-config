module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util
import Util.Servant.Curl

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.Client (BaseUrl (..), Scheme (Http))
import Servant.Foreign (HasForeignType, typeFor)
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import Util.Servant.Streamly qualified as Servant
import Util.Util

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    , curlDocsCallback :: Text -> IO ()
    }

type R = Get '[PlainText] Text
data Routes mode = Routes
    { resetError :: mode :- "reset-error" :> R
    , exitSuccess :: mode :- "exit" :> R
    , exitFailure :: mode :- "exit" :> Capture "code" Int :> R
    , getLightPower :: mode :- "get-light-power" :> Capture "light" (Exists' RoomLightPair) :> R
    , setLightPower :: mode :- "set-light-power" :> Capture "light" (Exists' RoomLightPair) :> Capture "power" Bool :> R
    , getLightColour :: mode :- "get-light-colour" :> Capture "light" (Exists' RoomLightPair) :> R
    , toggleLight :: mode :- "toggle-light" :> Capture "light" (Exists' RoomLightPair) :> R
    , setLightColourBK ::
        mode
            :- "set-light-colour"
                :> Capture "light" (RoomLightPair KelvinOnly)
                :> Capture "delay" NominalDiffTime
                :> Capture "brightness" Word16
                :> Capture "kelvin" Kelvin
                :> R
    , setLightColour ::
        mode
            :- "set-light-colour"
                :> Capture "light" (RoomLightPair FullColours)
                :> Capture "delay" NominalDiffTime
                :> Capture "hue" Word16
                :> Capture "saturation" Word16
                :> Capture "brightness" Word16
                :> Capture "kelvin" Kelvin
                :> R
    , setDeskPower :: mode :- "set-desk-power" :> Capture "device" DeskPowerDevice :> Capture "power" Bool :> R
    , sendEmail :: mode :- "send-email" :> Capture "subject" Text :> Capture "body" Text :> R
    , setOtherLED :: mode :- "set-other-led" :> Capture "power" Bool :> R
    , setSystemLEDs :: mode :- "set-system-leds" :> Capture "power" Bool :> R
    , sleepOrWake :: mode :- "sleep-or-wake" :> R
    , lightsOut :: mode :- "lights-out" :> R
    }
    deriving (Generic)

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Servant.stream @(NamedRoutes Routes)
            Servant.Opts
                { warpSettings =
                    Warp.setBeforeMainLoop
                        (opts.curlDocsCallback $ curlDocs opts.port)
                        $ Warp.setPort opts.port Warp.defaultSettings
                , routes = \act ->
                    Routes
                        { resetError = f showT act $ send ResetError
                        , exitSuccess = f showT act . send $ Exit ExitSuccess
                        , exitFailure = f showT act . send . Exit . ExitFailure
                        , getLightPower = withExists $ f showT act . send . GetLightPower
                        , setLightPower = withExists $ f showT act . send .: SetLightPower
                        , getLightColour = withExists $ f showT act . send . GetLightColour
                        , toggleLight = withExists $ f showT act . toggleLight
                        , setLightColourBK = \lightBK delay brightness (Kelvin kelvin) ->
                            f showT act $ send SetLightColourBK{..}
                        , setLightColour = \light delay hue saturation brightness (Kelvin kelvin) ->
                            f showT act $ send SetLightColour{colour = HSBK{..}, ..}
                        , setDeskPower = \device power -> f showT act . send $ SetDeskPower device power
                        , sendEmail = \subject body -> f showT act $ send SendEmail{..}
                        , setOtherLED = f showT act . send . SetOtherLED
                        , setSystemLEDs = f showT act . send . SetSystemLEDs
                        , sleepOrWake = f showT act $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                        , lightsOut = f showT act $ traverse_ (withExists' $ send . flip SetLightPower False) enumerateRoomLights
                        }
                }
            <&> \case
                Servant.Event x -> Just [x]
                Servant.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    -- Enqueue the action, then block until the event loop tells us how it went.
    -- An `Error` has already been logged and lit the error LED by that point, but we still need to
    -- answer the HTTP request, so we turn it in to a 500 rather than leaving the client hanging.
    f show' (act :: Event -> IO ()) a = do
        r <- liftIO do
            m <- newEmptyMVar
            act $ ActionEvent (putMVar m) a
            takeMVar m
        either
            (\e -> throwError err500{errBody = BL.fromStrict . encodeUtf8 $ renderError e})
            (pure . (<> "\n") . show')
            r

curlDocs :: Int -> Text
curlDocs port =
    T.intercalate "\n" $
        zipWith
            (\v es -> T.unlines $ v : es)
            (curlFunctions host api)
            (curlExamples host api)
  where
    api = Proxy @(NamedRoutes Routes)
    host =
        BaseUrl
            { baseUrlScheme = Http
            , baseUrlHost = "clark"
            , baseUrlPort = port
            , baseUrlPath = ""
            }

-- TODO use more newtypes like this in `lifx-lan` itself?
-- this is the important one, since the values aren't meaningful across the whole `Word16` range
newtype Kelvin = Kelvin Word16 deriving newtype (ToHttpApiData, FromHttpApiData)
instance HasForeignType Curl Examples Kelvin where
    typeFor = typeForExamples $ map Kelvin [2700, 3500]
