{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Lifx where

import Lifx.Lan

import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Log (LoggingT, MonadLog)
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Network.Socket (PortNumber)

-- I really don't know where this belongs - standalone package?
instance (MonadLifx m) => MonadLifx (LoggingT t m) where
    type MonadLifxError (LoggingT t m) = MonadLifxError m
    liftProductLookupError = liftProductLookupError @m
    sendMessage = let (.:) = (.) . (.) in lift .: sendMessage
    broadcastMessage = lift . broadcastMessage
    discoverDevices = lift . discoverDevices
    lifxThrow = lift . lifxThrow
instance (MonadLog s m) => MonadLog s (LifxT m)

statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

-- lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: (MonadIO m) => (Either e LifxError -> m ()) -> Int -> Maybe PortNumber -> ExceptT e (LifxT m) a -> m a
runLifxUntilSuccess p t n x =
    either (p' . Right) (either (p' . Left) pure)
        =<< runLifxT t n (runExceptT x)
  where
    p' e = p e >> liftIO (threadDelay t) >> runLifxUntilSuccess p t n x
