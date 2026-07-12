module George.Feed.GPIO (feed, Opts (..)) where

import George.Core

import Control.Monad.Freer (send)
import Control.Monad.Freer.Error (throwError)
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S
import Util.Streamly.GPIO qualified as GPIO

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: NominalDiffTime
    , window :: NominalDiffTime
    }

feed :: Opts -> S.Stream IO [Event]
feed Opts{..} =
    ( \case
        1 -> [send ResetError]
        3 -> [send PowerOff]
        n -> [throwError $ Error "No action for this number of GPIO presses" n]
    )
        . length @[] @()
        . toList
        <$> S.groupByTime window (GPIO.stream GPIO.Opts{..})
