module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.Functor
import Data.Text (Text)
import Data.Time
import Data.Tuple.Extra
import Data.Word
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App hiding (body)
import Okapi.Response
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import Util.Streamly qualified as S
import Util.Util hiding ((.:))

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        stream' @IO @Event
            Opts'
                { warpSettings = Warp.setPort opts.port Warp.defaultSettings
                , routes =
                    [ \emit -> lit "reset-error" . wrap $ f showT emit $ send ResetError
                    , \emit -> lit "exit" . param . wrap $ f showT emit . send . Exit . maybe ExitSuccess ExitFailure
                    , \emit -> lit "get-light-power" . param . wrap . withExists' $ f showT emit . send . GetLightPower
                    , \emit -> lit "set-light-power" . param . param . wrap . withExists' $ f showT emit . send .: SetLightPower
                    , \emit -> lit "get-light-colour" . param . wrap . withExists' $ f showT emit . send . GetLightColour
                    , \emit ->
                        lit "set-light-colour" $
                            choice
                                [ param . param . param . param $
                                    wrap \light delay brightness kelvin ->
                                        f showT emit $ send SetLightColourBK{lightBK = light, ..}
                                , param . param . param . param . param . param $
                                    wrap \light delay hue saturation brightness kelvin ->
                                        f showT emit $ send SetLightColour{colour = HSBK{..}, ..}
                                ]
                    , \emit -> lit "set-desk-usb-power" . param . wrap $ f showT emit . send . SetDeskUSBPower
                    , \emit -> lit "send-email" . param . param $ wrap \subject body -> f showT emit $ send SendEmail{..}
                    , \emit -> lit "suspend-laptop" . wrap . f showT emit $ send SuspendLaptop
                    , \emit -> lit "set-other-led" . param . wrap $ f showT emit . send . SetOtherLED
                    , \emit -> lit "set-system-leds" . param . wrap $ f showT emit . send . SetSystemLEDs
                    , \emit -> lit "toggle-ceiling-light" . wrap . f showT emit $ toggleCeilingLight
                    , \emit -> lit "sleep-or-wake" . wrap . f showT emit $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                    ]
                }
            <&> \case
                Event' x -> Just [x]
                WarpLog' r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f :: (Show r) => (r -> Text) -> (Event -> IO ()) -> CompoundAction r -> (Headers '[] -> Text -> Wai.Response) -> Wai.Request -> IO Wai.Response
    f show' emit a ok _req = do
        m <- newEmptyMVar
        emit $ ActionEvent (putMVar m) a
        ok noHeaders . (<> "\n") . show' <$> takeMVar m
    wrap = responder @200 @'[] @Text @Text . method GET id

data Opts' a = Opts'
    { warpSettings :: Warp.Settings -- TODO what if the settings passed in override the logger? just say not to in Haddocks?
    , routes :: [(a -> IO ()) -> Node '[]]
    }
data Item' a
    = Event' a
    | WarpLog' Wai.Request Status (Maybe Integer)
stream' ::
    (MonadIO m) =>
    Opts' a ->
    S.Stream m (Item' a)
stream' Opts'{..} = S.morphInner liftIO $ S.fromEmitter \f ->
    Warp.runSettings (Warp.setLogger (curry3 $ f . uncurry3 WarpLog') warpSettings)
        . withDefault (choice $ map ($ (f . Event')) routes)
        $ \_ resp -> resp $ Wai.responseLBS status404 [] "Not Found..."

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
