module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Data.Functor
import Data.Time
import Data.Word
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event, error, get, head)
import Okapi qualified hiding (Event, head)
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly.Okapi qualified as Okapi

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

-- TODO update to new Okapi as soon as it's released: https://github.com/monadicsystems/okapi/issues/30
feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Okapi.stream
            Okapi.Opts
                { warpSettings = Warp.setPort opts.port Warp.defaultSettings
                , routes =
            [ withGetRoute "reset-error" $ f showT $ send ResetError
            , withGetRoute "get-light-power" $ f showT . send . withExists @NullConstraint GetLightPower =<< segParam
            , withGetRoute "set-light-power" do
                Exists' l <- segParam
                p <- segParam
                f showT $ send $ SetLightPower l p
            , withGetRoute "get-light-colour" $ f showT . send . withExists @NullConstraint GetLightColour =<< segParam
            , withGetRoute "set-light-colour" do
                Exists' light <- segParam
                delay <- segParam @NominalDiffTime -- TODO why do we need this type app?
                case light of
                    Lamp -> do
                        hue <- segParam
                        saturation <- segParam
                        brightness <- segParam
                        kelvin <- segParam
                        f showT $ send SetLightColour{colour = HSBK{..}, ..}
                    Ceiling -> do
                        brightness <- segParam
                        kelvin <- segParam
                        f showT $ send SetLightColourBK{lightBK = light, ..}
            , withGetRoute "set-desk-usb-power" $ f showT . send . SetDeskUSBPower =<< segParam
            , withGetRoute "send-email" do
                subject <- segParam
                body <- segParam
                f showT $ send SendEmail{..}
            , withGetRoute "suspend-laptop" $ f showT $ send SuspendLaptop
            , withGetRoute "set-other-led" $ f showT . send . SetOtherLED =<< segParam
            , withGetRoute "set-system-leds" $ f showT . send . SetSystemLEDs =<< segParam
            , withGetRoute "toggle-ceiling-light" $ f showT toggleCeilingLight
            , withGetRoute "sleep-or-wake" $ f showT $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
            ]
                }
            <&> \case
                Okapi.Event x -> Just [x]
                Okapi.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' a = do
        m <- liftIO newEmptyMVar
        pure
            ( ActionEvent (putMVar m) a
            , okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)
            )
    withGetRoute s x = Okapi.get >> seg s >> x
