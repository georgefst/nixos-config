module Main (main) where

import George.Core
import George.Feed.GPIO qualified as GPIO
import George.Feed.UDP qualified as UDP
import George.Feed.WebServer qualified as WebServer
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.State
import Data.Bifunctor
import Data.Bool
import Data.ByteString qualified as B
import Data.Either.Extra
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as T
import Data.Time
import Data.Word
import Lifx.Lan qualified as Lifx
import Network.Socket (PortNumber)
import Network.Wai.Handler.Warp qualified as Warp
import Optics
import Optics.State.Operators
import Options.Generic
import Streamly.Data.Stream.Prelude qualified as S
import System.IO
import Text.Pretty.Simple
import Util

data Opts = Opts
    { gpioChip :: B.ByteString
    , buttonDebounce :: NominalDiffTime
    , buttonWindow :: NominalDiffTime
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledOtherPin :: Int
    , lifxTimeout :: Double
    , lifxPort :: Word16
    , receivePort :: PortNumber
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
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Clark"

    let
        setLED :: (MonadState AppState m, MonadIO m, MonadLog Text m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> #activeLEDs %= Map.delete pin
                    Nothing -> logMessage "LED is already off"
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= ((#activeLEDs %=) . Map.insert pin)
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
        $ runLifxUntilSuccess
            (either (handleError . Error "Lights not found" . toList @NonEmpty) (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
            do
                -- TODO this would be slightly cleaner if GHC were better about retaining polymorphism in do-bindings
                lightMap <- do
                    ds <- discoverLifx
                    let (notFound, ds') =
                            partitionEithers $
                                concatMap
                                    (\(Exists r) -> map (\(Exists l) -> (roomName r, lightName l)) $ enumerateLights r)
                                    enumerateRooms
                                    <&> \(r, l) ->
                                        uncurry (\t -> bimap (const t) (t,))
                                            . fmap (maybeToEither ())
                                            . ((r, l),)
                                            $ (ds & firstJust \(d, s, g) -> guard (g.label == r && s.label == l) $> d)
                    maybe (pure $ Map.fromList ds') (throwError @(NonEmpty (Text, Text))) $ nonEmpty notFound
                let getLight :: forall c. RoomLightPair c -> Lifx.Device
                    getLight (RoomLightPair r l) =
                        fromMaybe (error "light map not exhaustive") $
                            Map.lookup (roomName r, lightName l) lightMap
                runEventStream handleError logMessage (runAction (opts & \Opts{..} -> ActionOpts{..}))
                    . S.morphInner liftIO
                    $ S.parList
                        id
                        [ WebServer.feed $
                            opts & \Opts{..} -> WebServer.Opts{port = httpPort, ..}
                        , GPIO.feed $
                            opts
                                & \Opts
                                    { gpioChip = chip
                                    , buttonPin = pin
                                    , buttonDebounce = debounce
                                    , buttonWindow = window
                                    } -> GPIO.Opts{..}
                        , UDP.feed $
                            opts & \Opts{..} -> UDP.Opts{..}
                        ]
