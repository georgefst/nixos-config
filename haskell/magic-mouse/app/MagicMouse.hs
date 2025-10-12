module Main (main) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra (findM)
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Evdev
import Evdev.Codes (AbsoluteAxis (..), Key (BtnLeft, BtnMiddle, BtnRight), RelativeAxis (..), SyncEvent (SynReport))
import Evdev.Uinput qualified as U
import Optics
import Optics.State.Operators
import Options.Generic
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream qualified as S
import System.Directory.OsPath (listDirectory)
import System.Exit (exitFailure)
import System.OsPath ((</>))
import System.OsPath qualified as OsPath
import Prelude hiding (log)

data AppActiveState = AppActiveState
    { button :: Key
    , interruptedByScroll :: Bool
    , downFiredEarlyDueToDrag :: Bool
    }
    deriving (Eq, Ord, Show, Generic)

main :: IO ()
main = do
    dev <-
        listDirectory evdevDir'
            >>= traverse (newDevice' . (evdevDir' </>)) . filter (maybe False ("event" `isPrefixOf`) . OsPath.decodeUtf)
            >>= findM (fmap (== "Magic Mouse 2") . deviceName)
            >>= maybe (T.putStrLn "device not found" >> exitFailure) pure
    udev <- U.newDevice "magic-mouse-hack" U.defaultDeviceOpts{U.keys = [BtnLeft, BtnMiddle, BtnRight]}
    (either ((>> exitFailure) . T.putStrLn) pure =<<) $ runExceptT $ do
        log "starting"
        xAxisInfo <- maybe (throwError "no abs position") pure =<< liftIO (deviceAbsAxis dev AbsMtPositionX)
        flip evalStateT Nothing $ flip S.fold (readEventBatches dev) $ SF.drainMapM \evs ->
            case dropWhile (\case AbsoluteEvent AbsMtSlot _ -> True; _ -> False) $ map eventData evs of
                AbsoluteEvent AbsMtTrackingId (EventValue i) : evs' | i /= -1 -> do
                    log "down"
                    button <- case firstJust (\case AbsoluteEvent AbsMtPositionX (EventValue x) -> Just x; _ -> Nothing) evs' of
                        Nothing -> do
                            log "no x position found in batch with down event"
                            -- TODO is this a reasonable thing to do? I haven't really worked out why it happens
                            pure BtnLeft
                        Just x -> do
                            let r =
                                    (fromIntegral @_ @Double x - fromIntegral xAxisInfo.absMinimum)
                                        / (fromIntegral xAxisInfo.absMaximum - fromIntegral xAxisInfo.absMinimum)
                            -- TODO r can be greater than 1 due to x being higher than max
                            -- is this a driver issue? or is it that something like max+fuzz*resolution is allowed?
                            pure
                                if
                                    -- TODO intervals pretty arbitrary
                                    -- they feel about right but could certainly be honed further
                                    | r >= 0.65 -> BtnRight
                                    | r >= 0.42 -> BtnMiddle
                                    | otherwise -> BtnLeft
                    put $ Just AppActiveState{button, interruptedByScroll = False, downFiredEarlyDueToDrag = False}
                AbsoluteEvent AbsMtTrackingId (EventValue -1) : _ ->
                    get >>= \case
                        Just AppActiveState{..} -> do
                            put Nothing
                            if interruptedByScroll
                                -- TODO this seems an awkward case - requires more thought
                                -- NB. without this, the button may not be properly released if mouse and trackpad move
                                && not downFiredEarlyDueToDrag
                                then log "ignored"
                                else do
                                    log "up"
                                    unless downFiredEarlyDueToDrag $ down udev button
                                    up udev button
                        Nothing -> log "up event without down"
                evs' ->
                    get >>= \case
                        Just AppActiveState{..} ->
                            if
                                -- TODO what if both match? branch order ideally shouldn't matter here
                                | isJust $ find (relMatch [RelX, RelY]) evs' ->
                                    unless downFiredEarlyDueToDrag do
                                        log "dragging"
                                        down udev button
                                        _Just % #downFiredEarlyDueToDrag .= True
                                | isJust $ find (relMatch [RelWheelHiRes, RelHWheelHiRes, RelWheel, RelHwheel]) evs' ->
                                    _Just % #interruptedByScroll .= True
                                | otherwise -> pure ()
                          where
                            relMatch s = \case RelativeEvent a _ -> a `elem` s; _ -> False
                        _ -> pure ()
  where
    down udev button = liftIO $ U.writeBatch udev [KeyEvent button Pressed]
    up udev button = liftIO $ U.writeBatch udev [KeyEvent button Released]
    log s = liftIO do
        t <- T.pack . show <$> getCurrentTime
        T.putStrLn $ t <> T.replicate (36 - T.length t) " " <> s

-- TODO vendored / preview of upstream functionality
-- this program became a lot easier to write once we considered events in batches
-- which goes to show that it would be useful in library, as I've long suspected
readEventBatches :: (MonadIO m) => Device -> S.Stream m [Event]
readEventBatches = S.splitOn ((== SyncEvent SynReport) . eventData) SF.toList . readEvents
  where
    readEvents = S.repeatM . liftIO . nextEvent

-- TODO this is a pretty stupid way of doing this, but the upstream `evdev` definitions will use `OsPath` soon enough
osPathToBS :: OsPath.OsPath -> ByteString
osPathToBS = encodeUtf8 . T.pack . fromJust . OsPath.decodeUtf
osPathFromBS :: ByteString -> OsPath.OsPath
osPathFromBS = fromJust . OsPath.encodeUtf . T.unpack . decodeUtf8
evdevDir' :: OsPath.OsPath
evdevDir' = osPathFromBS evdevDir
newDevice' :: OsPath.OsPath -> IO Device
newDevice' = newDevice . osPathToBS
