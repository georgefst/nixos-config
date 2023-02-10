module GPIO where

import Util (showBS)

import Control.Monad.Except (MonadIO (..))
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Options.Generic (Text)
import RawFilePath (CreatePipe (CreatePipe), Inherit, Process, proc, processStdout, setStdout, startProcess)
import System.IO (hGetLine)

-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
gpioSet :: MonadIO m => [Int] -> m (Process Inherit Inherit Inherit)
gpioSet xs =
    liftIO
        . startProcess
        . proc "gpioset"
        $ "--mode=signal" : gpioChip : map ((<> "=1") . showBS) xs
gpioMon :: (Text -> IO ()) -> Double -> Int -> IO () -> IO ()
gpioMon putLine debounce pin x = do
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
gpioChip :: ByteString
gpioChip = "gpiochip0"
