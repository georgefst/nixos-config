module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Data.Foldable
import Data.Time
import Data.Tuple.Extra
import Data.Word
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event, error, get, head)
import Okapi qualified hiding (head)
import Options.Generic
import Streamly.Data.Stream.Prelude qualified as S

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    }

server :: Opts -> (forall m. (MonadIO m) => Event -> m ()) -> Wai.Application
server opts f =
    makeOkapiApp id $
        asum
            [ withGetRoute "reset-error" $ f2 $ send ResetError
            , withGetRoute "get-light-power" $ f1 showT . send . withExists GetLightPower =<< segParam
            , withGetRoute "set-light-power" $ do
                Exists l <- segParam
                p <- segParam
                f2 $ send $ SetLightPower l p
            , withGetRoute "get-light-colour" $ f1 showT . send . withExists GetLightColour =<< segParam
            , withGetRoute "set-light-colour" do
                Exists light <- segParam
                delay <- segParam @NominalDiffTime -- TODO why do we need this type app?
                case light of
                    Lamp -> do
                        hue <- segParam
                        saturation <- segParam
                        brightness <- segParam
                        kelvin <- segParam
                        f2 $ send SetLightColour{colour = HSBK{..}, ..}
                    Ceiling -> do
                        brightness <- segParam
                        kelvin <- segParam
                        f2 $ send SetLightColourBK{lightBK = light, ..}
            , withGetRoute "set-desk-usb-power" $ f2 . send . SetDeskUSBPower =<< segParam
            , withGetRoute "send-email" do
                subject <- segParam
                body <- segParam
                f2 $ send SendEmail{..}
            , withGetRoute "suspend-laptop" $ f2 $ send SuspendLaptop
            , withGetRoute "set-other-led" $ f2 . send . SetOtherLED =<< segParam
            , withGetRoute "set-system-leds" $ f2 . send . SetSystemLEDs =<< segParam
            , withGetRoute "toggle-ceiling-light" $ f2 toggleCeilingLight
            , withGetRoute "sleep-or-wake" $ f2 $ sleepOrWake opts.lifxMorningSeconds opts.lifxMorningKelvin
            ]
  where
    withGetRoute s x = Okapi.get >> seg s >> x
    f1 :: (Show a) => (a -> Text) -> CompoundAction a -> OkapiT IO Result
    f1 show' x = do
        m <- liftIO newEmptyMVar
        f $ ActionEvent (putMVar m) x
        okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)
    f2 x = f (ActionEvent mempty x) >> noContent []

feed :: (S.MonadAsync m) => Opts -> S.Stream m [Event]
feed opts = S.morphInner liftIO $ emitterToStream \f ->
    Warp.runSettings
        ( Warp.setLogger
            (curry3 $ unless . statusIsSuccessful . snd3 <*> f . pure . ErrorEvent . Error "HTTP error")
            . Warp.setPort opts.port
            $ Warp.defaultSettings
        )
        (server opts $ liftIO . f . pure)
