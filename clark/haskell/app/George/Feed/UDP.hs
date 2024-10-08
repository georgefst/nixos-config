module George.Feed.UDP (feed, Opts (..)) where

import George.Core

import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.Binary qualified as B
import Data.Binary.Get
import Data.Binary.Get qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding
import Data.Time
import Data.Tuple.Extra
import Data.Word
import Network.Socket
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly.UDP

data Opts = Opts
    { receivePort :: PortNumber --
    -- TODO these are the same as webserver - refactor somehow?
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

-- TODO re - evaluate this now that we have web API
decodeAction :: Opts -> BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) (CompoundAction ())
decodeAction opts =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ send ResetError
            1 -> pure $ toggleLight bedroom
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ send SendEmail{..}
            3 -> pure $ send SuspendLaptop
            5 -> send <$> (SetLightColourBK bedroom . secondsToNominalDiffTime <$> B.get <*> B.get <*> B.get)
            6 -> pure $ sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
            7 -> send . SetLightPower bedroom <$> B.get @Bool
            8 -> send . SetSystemLEDs <$> B.get @Bool
            n -> fail $ "unknown action: " <> show n
    where bedroom = RoomLightPair SBedroom BedroomLight

feed :: (MonadIO m) => Opts -> S.Stream m [Event]
feed opts =
    pure
        . either (ErrorEvent . Error "Decode failure") (ActionEvent mempty)
        . decodeAction opts
        <$> stream opts.receivePort
