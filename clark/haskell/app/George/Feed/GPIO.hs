module George.Feed.GPIO (feed, Opts (..)) where

import George.Core

import Control.Monad.Freer (send)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S
import Util.Streamly.GPIO qualified as GPIO
import Util.Util

data Opts = Opts
    { chip :: ByteString
    , pin :: Int
    , debounce :: NominalDiffTime
    , window :: NominalDiffTime
    }

feed :: Opts -> S.Stream IO [Event]
feed Opts{..} =
    uncurry
        ( \logLines ->
            (map LogEvent logLines <>) . \case
                0 -> [] -- TODO this batch only had ignored events - how can this happen unless `window < debounce`?
                1 -> [ActionEvent mempty $ send ResetError]
                3 -> [ActionEvent mempty $ send PowerOff]
                n -> [ErrorEvent $ Error "No action for this number of GPIO presses" n]
        )
        . second (length @[] @())
        . partitionEithers
        . toList
        . fmap \case
            GPIO.OutLine{ignoring, line} -> Left $ mwhen ignoring "(Ignoring) " <> line
            GPIO.Event -> Right ()
        <$> S.groupByTime window (GPIO.stream GPIO.Opts{..})
