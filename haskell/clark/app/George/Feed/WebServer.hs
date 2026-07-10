module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util
import Util.Servant.Curl

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.Client (BaseUrl (..), Scheme (Http))
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
                :> Capture "kelvin" Word16
                :> R
    , setLightColour ::
        mode
            :- "set-light-colour"
                :> Capture "light" (RoomLightPair FullColours)
                :> Capture "delay" NominalDiffTime
                :> Capture "hue" Word16
                :> Capture "saturation" Word16
                :> Capture "brightness" Word16
                :> Capture "kelvin" Word16
                :> R
    , setDeskPower :: mode :- "set-desk-power" :> Capture "device" DeskPowerDevice :> Capture "power" Bool :> R
    , sendEmail :: mode :- "send-email" :> Capture "subject" Text :> Capture "body" Text :> R
    , suspendLaptop :: mode :- "suspend-laptop" :> R
    , setOtherLED :: mode :- "set-other-led" :> Capture "power" Bool :> R
    , setSystemLEDs :: mode :- "set-system-leds" :> Capture "power" Bool :> R
    , sleepOrWake :: mode :- "sleep-or-wake" :> R
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
                        , setLightColourBK = \lightBK delay brightness kelvin ->
                            f showT act $ send SetLightColourBK{..}
                        , setLightColour = \light delay hue saturation brightness kelvin ->
                            f showT act $ send SetLightColour{colour = HSBK{..}, ..}
                        , setDeskPower = \device power -> f showT act . send $ SetDeskPower device power
                        , sendEmail = \subject body -> f showT act $ send SendEmail{..}
                        , suspendLaptop = f showT act $ send SuspendLaptop
                        , setOtherLED = f showT act . send . SetOtherLED
                        , setSystemLEDs = f showT act . send . SetSystemLEDs
                        , sleepOrWake = f showT act $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                        }
                }
            <&> \case
                Servant.Event x -> Just [x]
                Servant.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' (act :: Event -> IO ()) a = liftIO do
        m <- newEmptyMVar
        act $ ActionEvent (putMVar m) a
        (<> "\n") . show' <$> takeMVar m

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
