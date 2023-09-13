module George.Feed.GPIO (feed, GPIO.Opts (..)) where

import George.Core

import Control.Monad.Freer (send)
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly.GPIO qualified as GPIO
import Util.Util

feed :: GPIO.Opts -> S.Stream IO [Event]
feed opts =
    flip S.mapM (GPIO.stream opts) \case
        GPIO.OutLine{ignoring, line} -> pure [LogEvent $ mwhen ignoring "(Ignoring) " <> line]
        GPIO.Event -> pure [ActionEvent mempty $ send ResetError]
