module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Text ()
import Data.Text.Encoding
import Data.Time
import Lifx.Lan hiding (SetColor)
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra
import Text.Pretty.Simple hiding (Blue, Color, Green, Red, Vivid)

--TODO rename fields when we have OverloadedRecordDot (GHC 9.2), and thus simplify the `ParseRecord` instance
data Opts = Opts
    { optButtonDebounce :: Double
    , optLightName :: Text
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers . drop 3
                }

data Action
    = ToggleLight

main :: IO ()
main = do
    Opts{..} <- getRecord "Clark"
    mvar <- newEmptyMVar

    let listenOnNetwork = forever do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet 56710 0
            bs <- recv sock 1
            withSGR' Blue $ BS.putStrLn $ "Received UDP message: " <> bs
            putMVar mvar ToggleLight

    let listenForButton = do
            putStrLn "Starting gpiomon process..."
            hs@(_, Just gpiomonStdout, _, _) <-
                createProcess
                    (proc "gpiomon" ["-b", "-f", "gpiochip0", "5"])
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

    let worker = runLifxUntilSuccess do
            devs <- discoverDevices Nothing
            devNames <- traverse (\d -> (d,) <$> sendMessage d GetColor) devs
            case find ((== encodeUtf8 optLightName) . label . snd) devNames of
                Nothing -> do
                    liftIO $ putStrLn "Couldn't find ceiling light, only:"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} devNames
                Just (light, _) ->
                    forever $
                        liftIO (takeMVar mvar) >>= \case
                            ToggleLight -> toggleLight light

    worker `concurrently_` listenOnNetwork `concurrently_` listenForButton

toggleLight :: MonadLifx m => Device -> m ()
toggleLight light = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower

--TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

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
runLifxUntilSuccess :: Lifx a -> IO a
runLifxUntilSuccess x = either (\e -> print e >> runLifxUntilSuccess x) pure =<< runLifxT 5_000_000 x
