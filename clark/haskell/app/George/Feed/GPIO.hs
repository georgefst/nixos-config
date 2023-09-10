module George.Feed.GPIO (feed, GPIO.Opts (..)) where

import George.Core

import Control.Monad.IO.Class
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly.GPIO qualified as GPIO
import Util.Util

feed :: GPIO.Opts -> S.Stream IO [Event]
feed opts =
    flip S.mapM (GPIO.stream opts) \case
        GPIO.OutLine{ignoring, line} -> pure [LogEvent $ mwhen ignoring "(Ignoring) " <> line]
        -- TODO we'd ideally use this is a power button, but for now we just monitor it
        -- since there have been issues with electrical interference causing spurious triggers
        GPIO.Event -> pure @[] . LogEvent . ("GPIO button pressed: " <>) . showT <$> liftIO getCurrentTime
