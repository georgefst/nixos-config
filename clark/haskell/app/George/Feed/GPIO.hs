module George.Feed.GPIO (feed, Opts (..)) where

import George.Core
import Util
import Util.GPIO qualified as GPIO

import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Streamly.Data.Stream.Prelude qualified as S

data Opts = Opts
    { gpioChip :: ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    }

feed :: (S.MonadAsync m) => Opts -> S.Stream m [Event]
feed opts = S.morphInner liftIO $ emitterToStream \f ->
    GPIO.mon
        opts.gpioChip
        (f . pure . LogEvent)
        opts.buttonDebounce
        opts.buttonPin
        (f [ActionEvent mempty $ send ResetError])
