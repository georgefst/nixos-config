module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Binary
import Data.Binary.Get
import Data.Bitraversable
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Either.Extra
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra
import Lifx.Lan hiding (SetColor)
import Network.HTTP.Client
import Network.Socket
import Network.Socket.ByteString
import Network.Wreq
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra
import Text.Pretty.Simple hiding (Color (..), Intensity (..)) -- TODO https://github.com/quchen/prettyprinter/issues/233

data Opts = Opts
    { buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , lightName :: Text
    , lifxTimeout :: Double
    , receivePort :: Word16
    , mailgunSandbox :: Text
    , mailgunKey :: Text
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers
                }

data Action
    = ResetError
    | ToggleLight
    | SendEmail {subject :: Text, body :: Text}
    deriving (Show)

main :: IO ()
main = do
    (opts :: Opts) <- getRecord "Clark"
    -- ensure all LEDs are off to begin with
    callProcess "gpioset" $ "gpiochip0" : map ((<> "=0") . show) [opts.ledErrorPin, opts.ledOtherPin]
    mvar <- newEmptyMVar
    -- TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
    let light = deviceFromAddress (192, 168, 1, 190)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral opts.receivePort) 0
            forever do
                bs <- recv sock 1
                withSGR' Blue $ BSC.putStrLn $ "Received UDP message: " <> bs
                let action = decodeAction $ BSL.fromStrict bs
                pPrint action -- TODO better logging
                maybe mempty (putMVar mvar) action

    let listenForButton = do
            putStrLn "Starting gpiomon process..."
            hs@(_, Just gpiomonStdout, _, _) <-
                createProcess
                    (proc "gpiomon" ["-b", "-f", "gpiochip0", show opts.buttonPin])
                        { std_out = CreatePipe
                        }
            putStrLn "Done!"

            handle (\(e :: IOException) -> print e >> cleanupProcess hs) $
                getCurrentTime >>= iterateM_ \t0 -> do
                    line <- hGetLine gpiomonStdout
                    t1 <- getCurrentTime
                    if diffUTCTime t1 t0 < realToFrac opts.buttonDebounce
                        then withSGR' Red $ putStr "Ignoring: "
                        else do
                            withSGR' Green $ putStr "Ok: "
                            putMVar mvar ToggleLight
                    putStrLn line
                    pure t1

    listenOnNetwork `concurrently_` listenForButton `concurrently_` runLifxUntilSuccess (lifxTime opts.lifxTimeout) do
        forever $
            liftIO (takeMVar mvar) >>= \case
                ResetError -> liftIO $ callProcess "gpioset" ["gpiochip0", show opts.ledErrorPin <> "=0"]
                ToggleLight -> toggleLight light
                SendEmail{subject, body} -> sendEmail opts subject body

decodeAction :: BSL.ByteString -> Maybe Action
decodeAction =
    fmap thd3 . eitherToMaybe . runGetOrFail do
        getWord8 >>= \case
            0 -> pure ResetError
            1 -> pure ToggleLight
            2 -> do
                (subject, body) <- bisequence $ dupe $ decodeUtf8 <$> Data.Binary.get
                pure $ SendEmail{..}
            n -> fail $ "unknown action: " <> show n

handleError :: Show a => Int -> Text -> a -> IO ()
handleError ledErrorPin t x = do
    T.putStrLn $ t <> ":"
    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} x
    callProcess "gpioset" ["gpiochip0", show ledErrorPin <> "=1"]

toggleLight :: MonadLifx m => Device -> m ()
toggleLight light = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower
sendEmail :: MonadIO m => Opts -> Text -> Text -> m ()
sendEmail opts subject body =
    liftIO $
        void (postWith postOpts url formParams)
            `catchHttpException` handleError opts.ledErrorPin "Failed to send email"
  where
    postOpts = defaults & auth ?~ basicAuth "api" (encodeUtf8 opts.mailgunKey)
    url = "https://api.milgun.net/v3/sandbox" <> T.unpack opts.mailgunSandbox <> ".mailgun.org/messages"
    formParams =
        [ "from" := "Mailgun Sandbox <postmaster@sandbox" <> opts.mailgunSandbox <> ".mailgun.org>"
        , "to" := ("George Thomas <georgefsthomas@gmail.com>" :: Text)
        , "subject" := subject
        , "text" := body
        ]
    catchHttpException = catchJust \case
        HttpExceptionRequest _ e -> Just e
        InvalidUrlException _ _ -> Nothing

-- TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

-- TODO lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

-- TODO upstream? not the first time I've defined this
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
