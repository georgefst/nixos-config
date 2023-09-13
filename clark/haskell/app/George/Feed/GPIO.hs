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
    ( \(logLines, repeats) ->
        map LogEvent logLines
            <> [LogEvent $ "GPIO button repeats: " <> showT repeats, ActionEvent mempty $ send ResetError]
    )
        . second (length @[] @())
        . partitionEithers
        . toList
        . fmap \case
            GPIO.OutLine{ignoring, line} -> Left $ mwhen ignoring "(Ignoring) " <> line
            GPIO.Event -> Right ()
        <$> S.groupByTime window (GPIO.stream GPIO.Opts{..})
