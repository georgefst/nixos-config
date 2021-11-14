module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS
import Data.Text ()
import Data.Time
import Data.Word
import Lifx.Lan hiding (SetColor)
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra

--TODO rename fields when we have OverloadedRecordDot (GHC 9.2), and thus simplify the `ParseRecord` instance
data Opts = Opts
    { optButtonDebounce :: Double
    , optButtonPin :: Int
    , optLightName :: Text
    , optLifxTimeout :: Double
    , optReceivePort :: Word16
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers . drop (length @[] "opt")
                }

data Action
    = ToggleLight

main :: IO ()
main = do
    Opts{..} <- getRecord "Clark"
    mvar <- newEmptyMVar
    --TODO avoid hardcoding - discovery doesn't currently work on Clark (known GHC 9.2.1 code aarch64 code gen bug?)
    let light = deviceFromAddress (192, 168, 1, 190)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral optReceivePort) 0
            forever do
                bs <- recv sock 1
                withSGR' Blue $ BS.putStrLn $ "Received UDP message: " <> bs
                putMVar mvar ToggleLight

    let listenForButton = do
            putStrLn "Starting gpiomon process..."
            hs@(_, Just gpiomonStdout, _, _) <-
                createProcess
                    (proc "gpiomon" ["-b", "-f", "gpiochip0", show optButtonPin])
                        { std_out = CreatePipe
                        }
            putStrLn "Done!"

            handle (\(e :: IOException) -> print e >> cleanupProcess hs) $
                getCurrentTime >>= iterateM_ \t0 -> do
                    line <- hGetLine gpiomonStdout
                    t1 <- getCurrentTime
                    if diffUTCTime t1 t0 < realToFrac optButtonDebounce
                        then withSGR' Red $ putStr "Ignoring: "
                        else do
                            withSGR' Green $ putStr "Ok: "
                            putMVar mvar ToggleLight
                    putStrLn line
                    pure t1

    listenOnNetwork `concurrently_` listenForButton `concurrently_` do
        forever do
            --TODO we should shift `runLifx` a level up to avoid recreating the context, but we hit a GHC bug:
            -- https://gitlab.haskell.org/ghc/ghc/-/issues/20673
            takeMVar mvar >>= \case
                ToggleLight -> runLifxUntilSuccess (lifxTime optLifxTimeout) $ toggleLight light

toggleLight :: MonadLifx m => Device -> m ()
toggleLight light = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower

--TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

--TODO lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

--TODO upstream? not the first time I've defined this
withSGR :: [SGR] -> IO a -> IO a
withSGR sgr x = do
    setSGR sgr
    r <- x
    setSGR [Reset]
    pure r

-- A special case of 'withSGR'
withSGR' :: MonadIO m => Color -> IO a -> m a
withSGR' x = liftIO . withSGR [SetColor Foreground Vivid x, SetConsoleIntensity BoldIntensity]

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: Int -> Lifx a -> IO a
runLifxUntilSuccess t x = either (\e -> print e >> threadDelay t >> runLifxUntilSuccess t x) pure =<< runLifxT t x
