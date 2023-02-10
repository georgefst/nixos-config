{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO upstream everything in this module
module Util.Lifx where

import Lifx.Lan
import Lifx.Lan.Internal (LifxT (LifxT))

import Control.Concurrent (threadDelay)
import Control.Monad.Log (LoggingT, MonadLog)
import Control.Monad.State (MonadState (state))
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans, lift)
import Control.Monad.Writer (WriterT)

-- I really don't know where this belongs - standalone package?
instance MonadLifx m => MonadLifx (LoggingT t m) where
    type MonadLifxError (LoggingT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance MonadLog s m => MonadLog s (LifxT m)

statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

-- lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: MonadIO m => (LifxError -> m ()) -> Int -> LifxT m a -> m a
runLifxUntilSuccess p t x = either (\e -> p e >> liftIO (threadDelay t) >> runLifxUntilSuccess p t x) pure =<< runLifxT t x

-- already upstreamed, just not yet in a release
instance (MonadLifx m, Monoid t) => MonadLifx (WriterT t m) where
    type MonadLifxError (WriterT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance MonadTrans LifxT where
    lift = LifxT . lift . lift . lift
instance MonadState s m => MonadState s (LifxT m) where
    state = lift . state
