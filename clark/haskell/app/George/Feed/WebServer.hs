module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Data.Text (Text)
import Data.Time
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
feed opts = S.fromEmitter \emit ->
    let warpLog r s i = unless (statusIsSuccessful s) $ emit [ErrorEvent (Error "HTTP error" (r, s, i))]
        f :: (Show r) => (r -> Text) -> CompoundAction r -> (Headers '[] -> Text -> Wai.Response) -> Wai.Request -> IO Wai.Response
        f show' a ok _req = do
            m <- newEmptyMVar
            emit $ pure $ ActionEvent (putMVar m) a
            ok noHeaders . (<> "\n") . show' <$> takeMVar m
        wrap = responder @200 @'[] @Text @Text . method GET id
     in Warp.runSettings (Warp.setLogger warpLog (Warp.setPort opts.port Warp.defaultSettings)) $
            withDefault
                ( choice
                    [ lit "reset-error" . wrap $ f showT $ send ResetError
                    , lit "exit" . param . wrap $ f showT . send . Exit . maybe ExitSuccess ExitFailure
                    , lit "get-light-power" . param . wrap . withExists' $ f showT . send . GetLightPower
                    , lit "set-light-power" . param . param . wrap . withExists' $ f showT . send .: SetLightPower
                    , lit "get-light-colour" . param . wrap . withExists' $ f showT . send . GetLightColour
                    , lit "set-light-colour" $
                        choice
                            [ param . param . param . param $
                                wrap \light delay brightness kelvin ->
                                    f showT $ send SetLightColourBK{lightBK = light, ..}
                            , param . param . param . param . param . param $
                                wrap \light delay hue saturation brightness kelvin ->
                                    f showT $ send SetLightColour{colour = HSBK{..}, ..}
                            ]
                    , lit "set-desk-usb-power" . param . wrap $ f showT . send . SetDeskUSBPower
                    , lit "send-email" . param . param $ wrap \subject body -> f showT $ send SendEmail{..}
                    , lit "suspend-laptop" . wrap . f showT $ send SuspendLaptop
                    , lit "set-other-led" . param . wrap $ f showT . send . SetOtherLED
                    , lit "set-system-leds" . param . wrap $ f showT . send . SetSystemLEDs
                    , lit "toggle-ceiling-light" . wrap . f showT $ toggleCeilingLight
                    , lit "sleep-or-wake" . wrap . f showT $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                    ]
                )
                \_req resp -> resp $ Wai.responseLBS status404 [] "Not Found..."

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
