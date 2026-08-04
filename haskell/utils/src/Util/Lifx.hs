{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Lifx where

import Lifx.Lan

import Control.Monad (filterM)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Log (LoggingT, MonadLog, logMessage)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Bool (bool)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Util.Util (threadDelay')

-- I really don't know where these belong - standalone package?
-- (both are one-liners because each class provides defaults for monad transformers)
instance (MonadLifx m) => MonadLifx (LoggingT t m)
instance (MonadLog s m) => MonadLog s (LifxT m)

statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

{- | Repeatedly run the action until it succeeds, reporting each failure and pausing in between.

A `Left` is a failure which the action itself diagnosed, a `LifxError` one thrown by `lifx-lan`.

This is for startup steps which can't sensibly proceed without a result - e.g. finding the lights
we're going to control, when we may well be starting before the network is up. Transient failures
during normal operation are handled by `lifx-lan`'s own retries, and then by `catchActionErrors`.
-}
retryUntilSuccess ::
    (MonadIO m, MonadCatch m) =>
    -- | report a failure
    (Either e LifxError -> m ()) ->
    -- | wait this long before trying again
    NominalDiffTime ->
    m (Either e a) ->
    m a
retryUntilSuccess report delay x =
    try x >>= \case
        Left e -> again $ Right e
        Right (Left e) -> again $ Left e
        Right (Right y) -> pure y
  where
    again e = report e >> liftIO (threadDelay' delay) >> retryUntilSuccess report delay x

discoverLifx :: (MonadLifx m) => m [(Device, LightState, StateGroup, Product)]
discoverLifx =
    traverse
        (\d -> (d,,,) <$> sendMessage d GetColor <*> sendMessage d GetGroup <*> getProductInfo d)
        =<< discoverDevices Nothing
