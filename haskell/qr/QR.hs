module Main (main) where

import Codec.Picture
import Codec.QRCode
import Codec.QRCode.JuicyPixels
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Exit (exitFailure)
import System.IO (hClose)
import System.Posix (mkstemps)
import System.Process (callProcess)

main :: IO ()
main = do
    img <- maybe (putStrLn "encoding error" >> exitFailure) pure . encode opts enc . T.strip =<< T.getContents
    (fp, h) <- mkstemps "/tmp/" ".png"
    BL.hPut h $ encodePng $ toImage border (size `div` qrImageSize img) img
    hClose h
    callProcess "xdg-open" [fp]

border, size :: Int
border = 2
size = 1000
opts :: QRCodeOptions
opts = defaultQRCodeOptions M
enc :: TextEncoding
enc = Iso8859_1OrUtf8WithoutECI
