{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Lifx where

import Lifx.Lan

import Control.Monad (filterM)
import Control.Monad.Catch (MonadCatch, displayException, try)
import Control.Monad.Log (LoggingT, MonadLog, logMessage)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Util.Util (showT, threadDelay')

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

{- | Find all devices on the network, and everything we want to know about each.

Note that this is a broadcast (governed by `broadcastTimeout`) followed by several directed
messages per device (governed by the much shorter `messageTimeout`). A device which answers the
broadcast but then fails to answer the follow-ups is skipped, rather than being allowed to abort
the whole scan - one unresponsive bulb shouldn't stop us finding all the others. This matters more
than it sounds: `messageTimeout` is deliberately short, so if this threw, a single bulb which is
switched off at the wall would be enough to make startup discovery fail forever.
-}
discoverLifx :: (MonadLifx m, MonadCatch m, MonadLog Text m) => m [(Device, LightState, StateGroup, Product)]
discoverLifx =
    fmap catMaybes
        . traverse
            ( \d ->
                try ((d,,,) <$> sendMessage d GetColor <*> sendMessage d GetGroup <*> getProductInfo d) >>= \case
                    Right x -> pure $ Just x
                    Left (e :: LifxError) -> do
                        logMessage $ "Ignoring LIFX device which didn't respond during scan: " <> showT d <> " - " <> T.pack (displayException e)
                        pure Nothing
            )
        =<< discoverDevices Nothing

{- | `discoverLifx`, minus any devices whose label appears in the given list, logging as we go.

Both Sol's startup and its re-scan need exactly this.
-}
discoverLifxExcept :: (MonadLifx m, MonadCatch m, MonadLog Text m) => [Text] -> m [(Device, LightState, StateGroup, Product)]
discoverLifxExcept ignore =
    filterM
        ( \(_, LightState{label}, _, _) ->
            let good = label `notElem` ignore
             in logMessage ("LIFX device " <> bool "ignored" "found" good <> ": " <> label) >> pure good
        )
        =<< discoverLifx
