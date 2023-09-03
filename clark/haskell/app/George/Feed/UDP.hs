module George.Feed.UDP (feed, Opts (..)) where

import George.Core
import Util

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
import Network.Socket.ByteString hiding (send)
import Streamly.Data.Stream.Prelude qualified as S

data Opts = Opts
    { receivePort :: PortNumber --
    -- TODO these are the same as webserver - refactor somehow?
    , lifxMorningSeconds :: Int
    , lifxMorningKelvin :: Word16
    }

-- TODO re - evaluate this now that we have web API
decodeAction :: Opts -> BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) (CompoundAction ())
decodeAction opts =
    fmap thd3 . runGetOrFail do
        B.get @Word8 >>= \case
            0 -> pure $ send ResetError
            1 -> pure toggleCeilingLight
            2 -> do
                subject <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word8)
                body <- decodeUtf8 <$> (B.getByteString . fromIntegral =<< B.get @Word16)
                pure $ send SendEmail{..}
            3 -> pure $ send SuspendLaptop
            4 -> send . SetDeskUSBPower <$> B.get @Bool
            5 -> send <$> (SetLightColourBK Ceiling . secondsToNominalDiffTime <$> B.get <*> B.get <*> B.get)
            6 -> pure $ sleepOrWake opts.lifxMorningSeconds opts.lifxMorningKelvin
            7 -> send . SetLightPower Ceiling <$> B.get @Bool
            8 -> send . SetSystemLEDs <$> B.get @Bool
            n -> fail $ "unknown action: " <> show n

feed :: (MonadIO m) => Opts -> S.Stream m [Event]
feed opts = S.morphInner liftIO $ streamWithInit
    (socket AF_INET Datagram defaultProtocol >>= \s -> bind s (SockAddrInet opts.receivePort 0) >> pure s)
    \sock ->
        S.repeatM $
            pure
                . either (ErrorEvent . Error "Decode failure") (ActionEvent mempty)
                . decodeAction opts
                . BSL.fromStrict
                <$> recv sock 4096
