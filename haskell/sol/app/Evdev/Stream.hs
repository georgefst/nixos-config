{- | Functions for working with streams of input events.
Unless stated otherwise, these functions will throw exceptions if the underlying C calls fail.
-}
module Evdev.Stream (
    allDevices,
    allEvents,
    makeDevices,
    newDevices,
    newDevices',
    readEvents,
    readEventsMany,
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Bitraversable (bisequence)
import Data.Bool
import Data.Either.Extra
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple.Extra (fst3, thd3, (&&&))
import Evdev
import RawFilePath.Directory
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Internal.Data.Stream qualified as SI
import System.FilePath.ByteString
import System.INotify qualified as INotify
import System.IO.Error

-- TODO provide a 'group' operation on streams, representing packets as sets

-- | Read all events from a device.
readEvents :: (MonadIO m) => Device -> SI.Stream m (Either IOError Event)
readEvents = SI.repeatM . liftIO . tryIOError . nextEvent

-- readEventsUntilError :: Device -> SI.Stream IO Event
-- readEventsUntilError = fmap (fromRight (error "can't happen")) . S.takeWhile isRight . readEvents
-- readEventsUntilError' :: Device -> SI.Stream IO (Either IOError Event) -- we know the only `Left` is the last element
-- readEventsUntilError' = takeUntil isRight . readEvents

{- | Concurrently read events from multiple devices.
Invariant: events for a device fall between appropriate added and removed events.
-}
readEventsMany :: (S.MonadAsync m) => SI.Stream m Device -> SI.Stream m (Device, Either (Either () IOError) Event)
-- TODO better to only end the stream only upon "No such device" rather than _all_ IO errors?
-- TODO use custom types to make this clearer? stream consists of `(Device, Conn | Disconn IOError | Event Event)`
-- TODO generalise to allow a `(Device, a)` stream as input? or can this be achieved with transformers?
readEventsMany =
    S.parConcatMap id \d ->
        fmap (d,)
            . S.cons (Left (Left ()))
            . fmap (first Right)
            . takeUntil isLeft
            $ readEvents d

-- | Create devices for all paths in the stream.
makeDevices :: (MonadIO m) => S.Stream m RawFilePath -> S.Stream m (Either (RawFilePath, IOError) Device)
makeDevices = S.mapM $ liftIO . fmap (uncurry $ first . (,)) . bisequence . (pure &&& tryIOError . newDevice)

-- | All events on all devices. Silently discards devices on error
allEvents :: (S.MonadAsync m) => S.Stream m (Device, Event)
-- TODO do I want more or less of these sorts of wrappers?
allEvents = S.mapMaybe (traverse eitherToMaybe) $ readEventsMany $ S.mapMaybe eitherToMaybe allDevices

-- TODO call this 'oldDevices' or 'existingDevices', and have 'allDevices' include 'newDevices'?

-- | All existing devices.
allDevices :: (MonadIO m) => SI.Stream m (Either (RawFilePath, IOError) Device)
allDevices =
    let paths =
            S.filterM (liftIO . doesFileExist) $
                fmap (evdevDir </>) $
                    SI.unCross $
                        (SI.mkCross . S.fromList)
                            =<< SI.mkCross (S.fromEffect (reverse <$> liftIO (listDirectory evdevDir)))
     in makeDevices paths

-- TODO perhaps streamly-fsnotify ought to use RawFilePath?
-- TODO fix this - we don't always seem to get notified of permission changes -
-- indeed when we don't, we actually find that 'stat' and 'ls -l' show different permissions to:
-- 'fmap (flip showOct "" . fileMode) . getFileStatus'

-- {- | All new devices created (in /\/dev\/input/).
-- Watches for new file paths (using \inotify\), and those corresponding to valid devices are added to the stream.
-- -}
newDevices :: (MonadIO m) => S.Stream m Device
newDevices =
    let
        -- 'watching' keeps track of the set of paths which have been added, but don't yet have the right permissions
        watch :: (MonadIO m) => Set RawFilePath -> INotify.Event -> m (Maybe Device, Set RawFilePath)
        watch watching =
            liftIO . \case
                INotify.Created{isDirectory = False, filePath = p} ->
                    tryNewDevice p <&> \case
                        Right d ->
                            -- success - return new device
                            (Just d, watching)
                        Left e ->
                            -- TODO expose this to be logged somehow
                            -- fail - if it's only a permission error then watch for changes on device
                            (Nothing, applyWhen (isPermissionError e) (Set.insert p) watching)
                INotify.Modified{isDirectory = False, maybeFilePath = Just p} ->
                    if p `elem` watching
                        then
                            tryNewDevice p <&> \case
                                Right d ->
                                    -- success - no longer watch for changes
                                    (Just d, Set.delete p watching)
                                Left _ ->
                                    -- fail - continue to watch
                                    (Nothing, watching)
                        else -- this isn't an event we care about
                            return (Nothing, watching)
                INotify.Deleted{isDirectory = False, filePath = p} ->
                    -- device is gone - no longer watch for changes
                    return (Nothing, Set.delete p watching)
                _ -> return (Nothing, watching)
        tryNewDevice = tryIOError . newDevice
     in
        scanMaybe watch Set.empty $ watchDirectory evdevDir

-- TODO just fix 'newDevices'

{- | This is a workaround for bugginess in 'newDevices' when it comes to waiting for permissions on a new device
- it just waits the number of microseconds given before trying to read from the device.
-}
newDevices' :: (MonadIO m) => Int -> SI.Stream m (Either (RawFilePath, IOError) Device)
newDevices' delay = makeDevices $ flip S.mapMaybeM (watchDirectory evdevDir) \case
    INotify.Created False p -> do
        liftIO $ threadDelay delay
        pure $ Just $ evdevDir </> p
    _ -> return Nothing

-- {- Util -}

-- specialized form of S.scanlM'
-- for each a, f updates s, and possibly produces a new b, to add to the output stream
-- I really can't think of a good name for this...
-- TODO perhaps some way to use State monad instead?
scanMaybe :: (Monad m) => (s -> a -> m (Maybe b, s)) -> s -> S.Stream m a -> S.Stream m b
scanMaybe f e = S.mapMaybe fst . SI.scanlM' (f . snd) (pure (Nothing, e))

-- apply the function iff the guard passes
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

watchDirectory :: (MonadIO m) => RawFilePath -> S.Stream m INotify.Event
watchDirectory path = SI.unCross do
    -- TODO clean up with `removeWatch _wd` on exit somehow
    (m, _wd) <- SI.mkCross $ S.fromEffect $ liftIO do
        m <- newEmptyMVar
        inotify <- INotify.initINotify
        wd <- INotify.addWatch inotify [INotify.Create] path (putMVar m)
        pure (m, wd)
    SI.mkCross $ S.repeatM $ liftIO $ takeMVar m

-- TODO I suspect there's a better way to implement this
-- like `S.takeWhile . (not .)` but takes one more element
takeUntil :: (Monad m) => (a -> Bool) -> SI.Stream m a -> SI.Stream m a
takeUntil f =
    S.catMaybes
        . fmap thd3
        . S.takeWhile fst3
        . flip SI.scanl' (True, True, Nothing) \(_, oldWasGood, _) new -> (oldWasGood, not $ f new, Just new)
