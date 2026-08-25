{- HLINT ignore "Use <=<" -}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import API (Percentage)
import George.Core
import George.Feed.Keyboard qualified as Keyboard
import George.Feed.WebServer qualified as WebServer
import Util
import Util.GPIO.Persistent qualified as GPIO
import Util.Lifx

import Control.Exception (displayException)
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.State.Strict
import Data.Bool
import Data.List.Extra
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (NominalDiffTime)
import Data.Word
import Evdev.Codes (Key (..))
import Evdev.Uinput qualified as Uinput
import Lifx.Lan qualified as Lifx
import Network.HTTP.Client
import Network.Socket
import Network.Wai.Handler.Warp qualified as Warp
import Optics
import Options.Applicative qualified
import Options.Generic
import SigmaDSP qualified
import Streamly.Data.Stream.Prelude qualified as S
import System.IO
import System.OsString.Posix (PosixString, encodeUtf)
import Text.Pretty.Simple
import Util.Util

data Opts = Opts
    { gpioChip :: PosixStringWrapped
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledIdleModePin :: Int
    , ledSendingModePin :: Int
    , ledNormalModePin :: Int
    , ledTvModePin :: Int
    , lifxMessageTimeout :: NominalDiffTime
    -- ^ How long to wait for one bulb to answer. This is on the critical path for every web
    -- request, so it wants to be just comfortably above the round trip time to a bulb.
    , lifxBroadcastTimeout :: NominalDiffTime
    -- ^ How long to spend collecting responses when scanning for bulbs. Always elapses in full,
    -- so it only affects startup and explicit re-scans, never normal operation.
    , lifxRetryDelay :: NominalDiffTime
    -- ^ How long to wait before retrying the initial scan, when we can't find any bulbs at all.
    , lifxIgnore :: [Text]
    , lifxPort :: Word16
    , httpPort :: Warp.Port
    , webRoot :: FilePath
    , keyboardNames :: [Text]
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    , hifiPlugIp :: IP
    , irConfigDir :: Text
    , dspDevice :: FilePath
    , spdifVolumeRegister :: SigmaDSP.Address
    , spdifMuteRegister :: SigmaDSP.Address
    , spdifVolumeStep :: Percentage
    }
    deriving (Show, Generic)
newtype PosixStringWrapped = PosixStringWrapped {unwrap :: PosixString}
    deriving newtype (Show)
instance ParseRecord PosixStringWrapped where
    parseRecord = getOnly <$> parseRecord
instance ParseField PosixStringWrapped where
    readField = maybe (fail "decode error") (pure . PosixStringWrapped) . encodeUtf =<< Options.Applicative.str
    metavar _ = "PATH"
instance ParseRecord Percentage where
    parseRecord = getOnly <$> parseRecord
instance ParseField Percentage
instance ParseFields PosixStringWrapped
instance ParseFields Percentage
instance ParseRecord SigmaDSP.Address where
    parseRecord = getOnly <$> parseRecord
instance ParseField SigmaDSP.Address
instance ParseFields SigmaDSP.Address
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Sol"

    let
        setLED :: (MonadState AppState m, MonadIO m, MonadLog Text m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> modifying #activeLEDs (Map.delete pin)
                    Nothing -> logMessage $ "LED is already off: " <> showT pin
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip.unwrap [pin] >>= modifying #activeLEDs . Map.insert pin
                    Just _ -> logMessage $ "LED is already on: " <> showT pin
                )

        handleError :: (MonadIO m, MonadState AppState m, MonadLog Text m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

        modeLED = \case
            Keyboard.Idle -> Just opts.ledIdleModePin
            Keyboard.Quiet -> Nothing
            Keyboard.Sending -> Just opts.ledSendingModePin
            Keyboard.Normal -> Just opts.ledNormalModePin
            Keyboard.TV -> Just opts.ledTvModePin
        initialMode = Keyboard.Idle

        isKeyboardName s = any (`T.isInfixOf` s) ("Keyboard" : opts.keyboardNames)

        lifxConfig =
            Lifx.defaultLifxConfig
                { Lifx.messageTimeout = opts.lifxMessageTimeout
                , Lifx.broadcastTimeout = opts.lifxBroadcastTimeout
                , Lifx.port = Just $ fromIntegral opts.lifxPort
                }

    initialState <- do
        httpConnectionManager <- newManager defaultManagerSettings
        keySendSocket <- socket AF_INET Datagram defaultProtocol >>= \s -> bind s (SockAddrInet defaultPort 0) >> pure s
        -- TODO shift this in to the LIFX block below - currently awkward because this is needed to run the state monad.
        -- it's at least no longer a problem to reuse the port, now that `runLifxT` closes its socket on the way out
        bulbs <-
            flip runLoggingT T.putStrLn
                . retryUntilSuccess
                    (logMessage . ("LIFX startup error: " <>) . either id (T.pack . displayException))
                    opts.lifxRetryDelay
                . Lifx.runLifxT lifxConfig
                $ ( \case
                        [] -> Left "no valid LIFX devices found"
                        ds -> Right . Map.fromList $ map ((\b -> (b.device, b)) . mkBulbEntry) ds
                  )
                    <$> discoverLifxExcept opts.lifxIgnore
        uinput <-
            liftIO $
                Uinput.newDevice
                    "sol-hs"
                    Uinput.defaultDeviceOpts
                        { Uinput.keys =
                            -- TODO copied from WebServer.hs
                            -- this is essentially:
                            -- map apiToEvdevKey enumerate
                            -- we can't even import that due to cyclicity
                            -- but we should rethink this anyway
                            [ KeyEnter
                            , KeyUp
                            , KeyDown
                            , KeyLeft
                            , KeyRight
                            , KeyEsc
                            , KeySpace
                            , KeyTab
                            , KeyLeftmeta
                            ]
                        }
        pure
            AppState
                { activeLEDs = mempty
                , currentLight = Nothing
                , lightColourCache = Nothing
                , ..
                }

    flip evalStateT initialState
        . flip runLoggingT (liftIO . T.putStrLn)
        -- no error handling here: `catchActionErrors` deals with LIFX failures per-action, so
        -- anything reaching this point is unrecoverable and we may as well let systemd restart us
        . Lifx.runLifxT lifxConfig
        . runEventStream handleError logMessage (runAction (opts & \Opts{..} -> ActionOpts{..}))
        . S.morphInner liftIO
        . S.append
            -- flash all lights to show we have finished initialising
            ( S.fromList
                . map (pure . ActionEvent mempty . send)
                $ concatMap
                    (\n -> [SetLED n True, Sleep 0.2, SetLED n False])
                    (mapMaybe modeLED enumerate <> [opts.ledErrorPin])
                    <> [Sleep 0.5]
                    <> maybe mempty (pure . flip SetLED True) (modeLED initialMode)
            )
        $ S.parList
            id
            [ let Opts{spdifVolumeStep} = opts in Keyboard.feed Keyboard.Opts{..}
            , WebServer.feed WebServer.Opts{port = opts.httpPort, curlDocsCallback = T.putStrLn, webRoot = opts.webRoot, doorbellSound = "/home/gthomas/doorbell.wav"} -- TODO temporary doorbell sound location for testing - should ultimately be a Nix store path
            -- TODO disabled until logging is better
            -- it's easier to see events when monitoring through a separate script
            -- , GPIO.feed (opts & \Opts{..} -> GPIO.Opts{..})
            ]
