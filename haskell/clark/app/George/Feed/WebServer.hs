module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util
import Util.Servant.Curl

import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Freer.Error qualified as Freer
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
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

-- hmm, what about future Haskell consumers?
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

feed :: Handle -> Opts -> S.Stream IO [Event]
feed (Handle submit) opts =
    S.catMaybes $
        Servant.stream @(NamedRoutes Routes)
            Servant.Opts
                { warpSettings =
                    Warp.setBeforeMainLoop
                        (opts.curlDocsCallback $ curlDocs opts.port)
                        $ Warp.setPort opts.port Warp.defaultSettings
                , routes =
                    Routes
                        { resetError = f $ send ResetError
                        , exitSuccess = f . send $ Exit ExitSuccess
                        , exitFailure = f . send . Exit . ExitFailure
                        , getLightPower = withExists $ f . send . GetLightPower
                        , setLightPower = withExists $ f . send .: SetLightPower
                        , getLightColour = withExists $ f . send . GetLightColour
                        , toggleLight = withExists $ f . toggleLight
                        , setLightColourBK = \lightBK delay brightness (Kelvin kelvin) ->
                            f $ send SetLightColourBK{..}
                        , setLightColour = \light delay hue saturation brightness (Kelvin kelvin) ->
                            f $ send SetLightColour{colour = HSBK{..}, ..}
                        , setDeskPower = \device power -> f . send $ SetDeskPower device power
                        , sendEmail = \subject body -> f $ send SendEmail{..}
                        , setOtherLED = f . send . SetOtherLED
                        , setSystemLEDs = f . send . SetSystemLEDs
                        , sleepOrWake = f $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                        , lightsOut = f $ traverse_ (withExists' $ send . flip SetLightPower False) enumerateRoomLights
                        }
                }
            <&> \case
                Servant.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [Freer.throwError $ Error "HTTP error" (r, s, i)]
  where
    f :: forall a. (Show a) => CompoundAction a -> ExceptT ServerError IO Text
    f a =
        liftIO (submit $ reinterpret3 send a) >>= \case
            Right r -> pure $ showT r <> "\n"
            Left e ->
                throwError
                    err500
                        { errBody =
                            TLE.encodeUtf8 . TL.fromStrict $ case e of
                                Error{title, body} -> title <> ": " <> showT body
                                SimpleError t -> t
                        }

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
