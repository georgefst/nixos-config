{- HLINT ignore "Redundant <&>" -}
module Main (main) where

import George.Core
import George.Feed.GPIO qualified as GPIO
import George.Feed.WebServer qualified as WebServer
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Monad
import Control.Monad.Error.Class (throwError)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.State
import Data.Bool
import Data.Either.Extra
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Text.IO qualified as T
import Data.Time
import Data.Word
import Lifx.Lan qualified as Lifx
import Network.Wai.Handler.Warp qualified as Warp
import Optics
import Optics.State.Operators
import Options.Applicative qualified
import Options.Generic
import Streamly.Data.Stream.Prelude qualified as S
import System.IO
import System.OsString.Posix (PosixString, encodeUtf)
import Text.Pretty.Simple
import Util

data Opts = Opts
    { gpioChip :: PosixStringWrapped
    , noGpio :: Bool
    , buttonDebounce :: NominalDiffTime
    , buttonWindow :: NominalDiffTime
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , lifxMessageTimeout :: NominalDiffTime
    -- ^ How long to wait for one bulb to answer, before giving up on that request.
    , lifxBroadcastTimeout :: NominalDiffTime
    -- ^ How long to spend collecting responses when scanning for bulbs. Always elapses in full.
    , lifxRetryDelay :: NominalDiffTime
    -- ^ How long to wait before retrying the initial scan, when we can't find all our lights.
    , lifxPort :: Word16
    , httpPort :: Warp.Port
    , emailPipe :: FilePath
    , laptopHostName :: Text
    , sshTimeout :: Int
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    , systemLedPipe :: FilePath
    , powerOffPipe :: FilePath
    }
    deriving (Show, Generic)
newtype PosixStringWrapped = PosixStringWrapped {unwrap :: PosixString}
    deriving newtype (Show)
instance ParseRecord PosixStringWrapped where
    parseRecord = getOnly <$> parseRecord
instance ParseField PosixStringWrapped where
    readField = maybe (fail "decode error") (pure . PosixStringWrapped) . encodeUtf =<< Options.Applicative.str
    metavar _ = "PATH"
instance ParseFields PosixStringWrapped
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Clark"

    let
        setLED :: (MonadState AppState m, MonadIO m, MonadLog Text m) => Int -> Bool -> m ()
        setLED pin =
            unless opts.noGpio
                . bool
                    ( use #activeLEDs <&> Map.lookup pin >>= \case
                        Just h -> GPIO.reset h >> #activeLEDs %= Map.delete pin
                        Nothing -> logMessage "LED is already off"
                    )
                    ( use #activeLEDs <&> Map.lookup pin >>= \case
                        Nothing -> GPIO.set opts.gpioChip.unwrap [pin] >>= ((#activeLEDs %=) . Map.insert pin)
                        Just _ -> logMessage "LED is already on"
                    )

        handleError :: (MonadIO m, MonadState AppState m, MonadLog Text m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

    flip evalStateT AppState{activeLEDs = mempty}
        . flip runLoggingT (liftIO . T.putStrLn)
        $ Lifx.runLifxT
            Lifx.defaultLifxConfig
                { Lifx.messageTimeout = opts.lifxMessageTimeout
                , Lifx.broadcastTimeout = opts.lifxBroadcastTimeout
                , Lifx.port = Just $ fromIntegral opts.lifxPort
                }
            do
                -- only the initial discovery is retried: we can't do anything at all until we've found
                -- every light we're configured to control, and we may be starting before they're up.
                -- transient failures later on are handled by `lifx-lan`'s retries and `catchActionErrors`
                -- TODO this would be slightly cleaner if GHC were better about retaining polymorphism in do-bindings
                lightMap <-
                    retryUntilSuccess
                        ( handleError . \case
                            Left ls -> Error "Lights not found" $ toList @NonEmpty ls
                            Right e -> Error "LIFX error during startup" e
                        )
                        opts.lifxRetryDelay
                        do
                            ds <- discoverLifx
                            let (notFound, ds') =
                                    partitionEithers $
                                        enumerateRoomLights <&> \(Exists (RoomLightPair (roomName -> r) (lightName -> l))) ->
                                            let rl = (r, l)
                                             in maybeToEither rl $
                                                    ds & firstJust \(d, s, g, _) -> guard (g.label == r && s.label == l) $> (rl, d)
                            pure $ maybe (Right $ Map.fromList ds') Left $ nonEmpty notFound
                            -- hmm, we can make this line almost the same as before
                            -- but something has changed in its effectfulness
                            -- pure $ maybe (pure $ Map.fromList ds') (throwError @(NonEmpty (Text, Text))) $ nonEmpty notFound
                let getLight :: forall c. RoomLightPair c -> Lifx.Device
                    getLight (RoomLightPair r l) =
                        fromMaybe (error "light map not exhaustive") $
                            Map.lookup (roomName r, lightName l) lightMap
                    Opts{..} = opts
                runEventStream handleError logMessage (runAction ActionOpts{..})
                    . S.morphInner liftIO
                    . S.parList id
                    $ mconcat
                        [ mwhen
                            (not opts.noGpio)
                            [ let
                                chip = gpioChip.unwrap
                                pin = buttonPin
                                debounce = buttonDebounce
                                window = buttonWindow
                               in
                                GPIO.feed GPIO.Opts{..}
                            ]
                        ,
                            [ WebServer.feed WebServer.Opts{port = httpPort, curlDocsCallback = T.putStrLn, ..}
                            ]
                        ]
