module MQTT.Meross (Message, send, toggle) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import GHC.Generics (Generic)
import RawFilePath (proc, readProcessWithExitCode)
import System.Exit (ExitCode)

send :: MonadIO m => Message -> m (ExitCode, ByteString, ByteString)
send m =
    liftIO . readProcessWithExitCode $
        proc
            "mosquitto_pub"
            [ "--insecure"
            , "--cafile"
            , "/syncthing/config/mqtt/certs/ca/ca.crt"
            , "-t"
            , "/appliance/1803194914121229080634298f154a3a/subscribe"
            , "-m"
            , BSL.toStrict $ encode m
            ]

toggle :: MonadIO m => Int -> Bool -> m Message
toggle channel onoff = do
    -- TODO we should be using a proper `timestamp` and a random `messageId`, but we need to regenerate `sign`...
    -- timestamp <- truncate <$> liftIO getPOSIXTime
    pure
        Message
            { header =
                -- these values came from a random online example somewhere - hey, they work...
                Header
                    { namespace = "Appliance.Control.ToggleX"
                    , method = "SET"
                    , from = "clark/haskell-script"
                    , payloadVersion = 1
                    , triggerSrc = "George"
                    , timestamp = 1601908439
                    , messageId = "ef6b8e50620ac768569f1f7abc6507a5"
                    , sign = "e48c24e510044d7e2d248c68ff2c10ca"
                    }
            , payload =
                Payload
                    { togglex =
                        Just
                            ToggleX
                                { channel
                                , onoff = fromEnum onoff
                                }
                    }
            }

data Message = Message
    { header :: Header
    , payload :: Payload
    }
    deriving (Generic, ToJSON)

data Header = Header
    { from :: Text
    , messageId :: Text
    , method :: Text
    , namespace :: Text
    , payloadVersion :: Int
    , sign :: Text
    , timestamp :: Int
    , triggerSrc :: Text
    }
    deriving (Generic, ToJSON)

{- HLINT ignore Payload "Use newtype instead of data" -}
data Payload = Payload
    { togglex :: Maybe ToggleX
    }
    deriving (Generic, ToJSON)

data ToggleX = ToggleX
    { channel :: Int
    , onoff :: Int
    }
    deriving (Generic, ToJSON)
