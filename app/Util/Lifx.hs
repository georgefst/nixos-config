{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO upstream everything in this module
module Util.Lifx where

import Lifx.Lan

import Control.Concurrent (threadDelay)
import Control.Monad.Log (LoggingT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)

-- I really don't know where this belongs - standalone package?
instance MonadLifx m => MonadLifx (LoggingT t m) where
    type MonadLifxError (LoggingT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow

statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

-- lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: Int -> Lifx a -> IO a
runLifxUntilSuccess t x = either (\e -> print e >> threadDelay t >> runLifxUntilSuccess t x) pure =<< runLifxT t x

-- already upstreamed, just not yet in a release
instance (MonadLifx m, Monoid t) => MonadLifx (WriterT t m) where
    type MonadLifxError (WriterT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
