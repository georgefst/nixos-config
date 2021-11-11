module Main (main) where

import Control.Concurrent.Extra
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Lifx.Lan hiding (SetColor)
import Network.Socket
import Network.Socket.ByteString
import System.Console.ANSI
import System.IO
import System.Process.Extra

main :: IO ()
main = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock $ SockAddrInet 56710 0
    void . forkIO . runLifxUntilSuccess $ forever do
        bs <- liftIO $ recv sock 1
        withSGR' Blue $ BS.putStrLn $ "Received UDP message: " <> bs
        toggleLight

    putStrLn "Starting gpiomon process..."
    hs@(_, Just gpiomonStdout, _, _) <-
        createProcess
            (proc "gpiomon" ["-b", "-f", "gpiochip0", "5"])
                { std_out = CreatePipe
                }
    putStrLn "Done!"

    handle (\(e :: IOException) -> print e >> cleanupProcess hs) . runLifxUntilSuccess $
        liftIO getCurrentTime >>= iterateM_ \t0 -> do
            line <- liftIO $ hGetLine gpiomonStdout
            t1 <- liftIO getCurrentTime
            if diffUTCTime t1 t0 < 0.5
                then withSGR' Red $ putStr "Ignoring: "
                else do
                    withSGR' Green $ putStr "Ok: "
                    toggleLight
            liftIO $ putStrLn line
            pure t1

light :: Device
light = deviceFromAddress (192, 168, 1, 190)

toggleLight :: MonadLifx m => m ()
toggleLight = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower

--TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

-- | A special case of 'withSGR'.
withSGR' :: MonadIO m => Color -> IO a -> m a
withSGR' x = liftIO . withSGR [SetColor Foreground Vivid x, SetConsoleIntensity BoldIntensity]

--TODO upstream? not the first time I've defined this
withSGR :: [SGR] -> IO a -> IO a
withSGR sgr x = do
    setSGR sgr
    r <- x
    setSGR [Reset]
    pure r

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: Lifx a -> IO a
runLifxUntilSuccess x = either (\e -> print e >> runLifxUntilSuccess x) pure =<< runLifxT 5_000_000 x
