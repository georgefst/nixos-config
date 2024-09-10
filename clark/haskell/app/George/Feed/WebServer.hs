module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Data.Functor
import Data.Text (Text)
import Data.Time
import Data.Word
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App hiding (body)
import Okapi.Response
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import Util.Streamly.Okapi qualified as Okapi
import Util.Util

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Okapi.stream
            Okapi.Opts
                { warpSettings = Warp.setPort opts.port Warp.defaultSettings
                , routes = \act ->
                    [ lit "reset-error" . simpleGet $ f showT act $ send ResetError
                    , lit "exit" . param . simpleGet $ f showT act . send . Exit . maybe ExitSuccess ExitFailure
                    , lit "get-light-power" . param . simpleGet . withExists' $
                        f showT act . send . GetLightPower
                    , lit "set-light-power" . param . param . simpleGet . withExists' $
                        f showT act . send .: SetLightPower
                    , lit "get-light-colour" . param . simpleGet . withExists' $
                        f showT act . send . GetLightColour
                    , lit "set-light-colour" $
                        choice
                            [ param . param . param . param $
                                simpleGet \light delay brightness kelvin ->
                                    f showT act $ send SetLightColourBK{lightBK = light, ..}
                            , param . param . param . param . param . param $
                                simpleGet \light delay hue saturation brightness kelvin ->
                                    f showT act $ send SetLightColour{colour = HSBK{..}, ..}
                            ]
                    , lit "set-desk-power" . param . param . simpleGet $ f showT act . send .: SetDeskPower
                    , lit "send-email" . param . param $ simpleGet \subject body -> f showT act $ send SendEmail{..}
                    , lit "suspend-laptop" . simpleGet . f showT act $ send SuspendLaptop
                    , lit "set-other-led" . param . simpleGet $ f showT act . send . SetOtherLED
                    , lit "set-system-leds" . param . simpleGet $ f showT act . send . SetSystemLEDs
                    , lit "toggle-ceiling-light" . simpleGet . f showT act $ toggleLight Ceiling
                    , lit "sleep-or-wake" . simpleGet . f showT act $
                        sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                    ]
                }
            <&> \case
                Okapi.Event x -> Just [x]
                Okapi.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' (act :: Event -> IO ()) a ok _req = do
        m <- newEmptyMVar
        act $ ActionEvent (putMVar m) a
        ok noHeaders . (<> "\n") . show' <$> takeMVar m
    simpleGet = responder @200 @'[] @Text @Text . method GET id
