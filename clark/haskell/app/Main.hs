module Main (main) where

import George.Core
import George.Feed.GPIO qualified as GPIO
import George.Feed.UDP qualified as UDP
import George.Feed.WebServer qualified as WebServer
import Util
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (logMessage, runLoggingT)
import Control.Monad.State
import Data.Bool
import Data.ByteString qualified as B
import Data.List
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Word
import Lifx.Lan qualified as Lifx
import Network.Wai.Handler.Warp qualified as Warp
import Optics
import Optics.State.Operators
import Options.Generic
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.IO
import Text.Pretty.Simple

data Opts = Opts
    { gpioChip :: B.ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , ceilingLightName :: Text
    , lampName :: Text
    , lifxTimeout :: Double
    , lifxPort :: Word16
    , receivePort :: Word16
    , httpPort :: Warp.Port
    , emailPipe :: FilePath
    , laptopHostName :: Text
    , sshTimeout :: Int
    , lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    , deskUsbPort :: Int
    , systemLedPipe :: FilePath
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Clark"
    eventMVar <- newEmptyMVar

    let
        setLED :: (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> #activeLEDs %= Map.delete pin
                    Nothing -> log' "LED is already off"
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= ((#activeLEDs %=) . Map.insert pin)
                    Just _ -> log' "LED is already on"
                )
          where
            log' = liftIO . putMVar eventMVar . LogEvent

        handleError :: (MonadIO m, MonadState AppState m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

    flip evalStateT AppState{activeLEDs = mempty}
        . flip runLoggingT (liftIO . T.putStrLn)
        $ runLifxUntilSuccess
            (either (handleError . Error "Light not found") (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
            do
                -- TODO this would be slightly cleaner if GHC were better about retaining polymorphism in do-bindings
                (ceilingLight, lamp) <- do
                    ds <- discoverLifx
                    let f name = maybe (throwError name) (pure . fst) $ find ((== name) . (.label) . snd) ds
                    (,) <$> f opts.ceilingLightName <*> f opts.lampName
                let getLight :: forall a. Light a -> Lifx.Device
                    getLight = \case
                        Ceiling -> ceilingLight
                        Lamp -> lamp
                S.fold
                    ( SF.drainMapM \case
                        ErrorEvent e -> handleError e
                        LogEvent t -> logMessage t
                        ActionEvent f action -> (either handleError pure <=< runExceptT) $ runM do
                            r <-
                                action & translate \a -> do
                                    logMessage $ showT a
                                    runAction (opts & \Opts{..} -> ActionOpts{..}) a
                            sendM . logMessage $ showT r
                            sendM . liftIO $ f r
                    )
                    . S.concatMap S.fromList
                    . S.cons [LogEvent "Starting..."]
                    . S.morphInner liftIO
                    $ S.parList
                        id
                        [ WebServer.feed (opts & \Opts{..} -> WebServer.Opts{port = httpPort, ..})
                        , GPIO.feed (opts & \Opts{..} -> GPIO.Opts{..})
                        , UDP.feed (opts & \Opts{..} -> UDP.Opts{..})
                        ]
