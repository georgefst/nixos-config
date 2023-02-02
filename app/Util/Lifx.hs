{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO upstream everything in this module
module Util.Lifx where

import Lifx.Lan

import Control.Monad.Log (LoggingT)
import Control.Monad.Trans (lift)

-- I really don't know where this belongs - standalone package?
instance MonadLifx m => MonadLifx (LoggingT t m) where
    type MonadLifxError (LoggingT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
