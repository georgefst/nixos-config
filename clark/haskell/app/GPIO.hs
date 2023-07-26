-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
module GPIO (Handle, reset, set, mon) where

import Util

import Control.Monad.Except (MonadIO (..))
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Options.Generic (Text)
import RawFilePath (CreatePipe (CreatePipe), Inherit, Process, proc, processStdout, setStdout, startProcess, terminateProcess)
import System.IO (hGetLine)

newtype Handle = Handle {unwrap :: Process Inherit Inherit Inherit}

reset :: Handle -> IO ()
reset h = terminateProcess h.unwrap

set :: (MonadIO m) => ByteString -> [Int] -> m Handle
set gpioChip xs =
    liftIO
        . fmap Handle
        . startProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . showBS) xs

mon :: ByteString -> (Text -> IO ()) -> Double -> Int -> IO () -> IO ()
mon gpioChip putLine debounce pin x = do
    p <-
        startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, showBS pin]
                `setStdout` CreatePipe
    getCurrentTime >>= iterateM_ \t0 -> do
        line <- hGetLine $ processStdout p
        t1 <- getCurrentTime
        if diffUTCTime t1 t0 < realToFrac debounce
            then putLine $ "(Ignoring) " <> T.pack line
            else putLine (T.pack line) >> x
        pure t1
