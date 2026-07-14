module George.Feed.GPIO (feed, Opts (..)) where

import George.Core

import Control.Monad.Freer (send)
import Data.Foldable
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S
import System.OsString.Posix (PosixString)
import Util.Streamly qualified as S
import Util.Streamly.GPIO qualified as GPIO

data Opts = Opts
    { chip :: PosixString
    , pin :: Int
    , debounce :: NominalDiffTime
    , window :: NominalDiffTime
    }

feed :: Opts -> S.Stream IO [Event]
feed Opts{..} =
    ( \case
        1 -> [ActionEvent mempty $ send ResetError]
        3 -> [ActionEvent mempty $ send PowerOff]
        n -> [ErrorEvent $ Error "No action for this number of GPIO presses" n]
    )
        . length @[] @()
        . toList
        <$> S.groupByTime window (GPIO.stream GPIO.Opts{..})
