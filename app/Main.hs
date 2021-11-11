module Main (main) where

import Control.Concurrent.Extra
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

main :: IO ()
main = do
    Opts{..} <- getRecord "Clark"
    Just light <- runLifxUntilSuccess do
        devs <- discoverDevices Nothing
        devNames <- traverse (\d -> (d,) <$> sendMessage d GetColor) devs
        pure $ fst <$> find ((== encodeUtf8 optLightName) . label . snd) devNames

    void . forkIO . runLifxUntilSuccess $ forever do
        sock <- liftIO $ socket AF_INET Datagram defaultProtocol
        liftIO $ bind sock $ SockAddrInet 56710 0
        bs <- liftIO $ recv sock 1
        withSGR' Blue $ BS.putStrLn $ "Received UDP message: " <> bs
        toggleLight light

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
            if diffUTCTime t1 t0 < realToFrac optButtonDebounce
                then withSGR' Red $ putStr "Ignoring: "
                else do
                    withSGR' Green $ putStr "Ok: "
                    toggleLight light
            liftIO $ putStrLn line
            pure t1

toggleLight :: MonadLifx m => Device -> m ()
toggleLight light = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower

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
