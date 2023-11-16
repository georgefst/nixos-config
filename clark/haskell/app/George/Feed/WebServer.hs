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
import Util.Util

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

feed :: Opts -> S.Stream IO [Event]
feed opts = S.fromEmitter \emit ->
    let warpLog r s i = unless (statusIsSuccessful s) $ emit [ErrorEvent (Error "HTTP error" (r, s, i))]
        f :: (Show r) => (Headers '[] -> Text -> Wai.Response) -> (r -> Text) -> CompoundAction r -> IO Wai.Response
        f ok show' a = do
            m <- newEmptyMVar
            emit $ pure $ ActionEvent (putMVar m) a
            ok noHeaders . (<> "\n") . show' <$> takeMVar m
        wrap = responder @200 @'[] @Text @Text . method GET id
     in Warp.runSettings (Warp.setLogger warpLog (Warp.setPort opts.port Warp.defaultSettings)) $
            withDefault
                ( choice
                    [ lit "reset-error" $
                        wrap \ok _req ->
                            f ok showT $ send ResetError
                    , lit "exit" . param $
                        wrap \c ok _req ->
                            f ok showT . send . Exit $ maybe ExitSuccess ExitFailure c
                    , lit "get-light-power" . param $
                        wrap \(Exists @NullConstraint l) ok _req ->
                            f ok showT . send $ GetLightPower l
                    , lit "set-light-power" . param . param $
                        wrap \(Exists @NullConstraint l) p ok _req ->
                            f ok showT . send $ SetLightPower l p
                    , lit "get-light-colour" . param $
                        wrap \(Exists @NullConstraint l) ok _req ->
                            f ok showT . send $ GetLightColour l
                    , lit "set-light-colour" $
                        choice
                            [ param . param . param . param $
                                wrap \light delay brightness kelvin ok _req ->
                                    f ok showT $ send SetLightColourBK{lightBK = light, ..}
                            , param . param . param . param . param . param $
                                wrap \light delay hue saturation brightness kelvin ok _req ->
                                    f ok showT $ send SetLightColour{colour = HSBK{..}, ..}
                            ]
                    , lit "set-desk-usb-power" . param $
                        wrap \p ok _req ->
                            f ok showT . send $ SetDeskUSBPower p
                    , lit "send-email" . param . param $
                        wrap \subject body ok _req ->
                            f ok showT $ send SendEmail{..}
                    , lit "suspend-laptop" $
                        wrap \ok _req ->
                            f ok showT $ send SuspendLaptop
                    , lit "set-other-led" . param $
                        wrap \p ok _req ->
                            f ok showT . send $ SetOtherLED p
                    , lit "set-system-leds" . param $
                        wrap \p ok _req ->
                            f ok showT . send $ SetSystemLEDs p
                    , lit "toggle-ceiling-light" $
                        wrap \ok _req ->
                            f ok showT toggleCeilingLight
                    , lit "sleep-or-wake" $
                        wrap \ok _req ->
                            f ok showT $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                    ]
                )
                \_req resp -> resp $ Wai.responseLBS status404 [] "Not Found..."
