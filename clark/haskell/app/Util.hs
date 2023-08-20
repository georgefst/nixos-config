{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Freer
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either.Extra
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Text.Lazy.Encoding qualified as Lazy
import Lifx.Lan
import Options.Generic
import RawFilePath
import Servant
import System.Exit
import Text.Pretty.Simple

showT :: (Show a) => a -> Text
showT = T.pack . show
showBS :: (Show a) => a -> ByteString
showBS = encodeUtf8 . showT

-- TODO return partial stdout/stderr in timeout case

{- | Essentially `\t -> timeout t . readProcessWithExitCode`, except that it actually works since it uses `SIGTERM`,
whereas `timeout` uses an async exception, and thus isn't good at terminating foreign code.
Time is in microseconds, as with `threadDelay` and `timeout`.
-}
readProcessWithExitCodeTimeout :: Int -> ProcessConf stdin stdout stderr -> IO (Maybe (ExitCode, ByteString, ByteString))
readProcessWithExitCodeTimeout t conf = do
    p <-
        startProcess $
            conf
                `setStdin` NoStream
                `setStdout` CreatePipe
                `setStderr` CreatePipe
    eitherToMaybe @() <$> ((threadDelay t >> terminateProcess p) `race` waitForProcess p)
        >>= traverse \exitCode -> (exitCode,,) <$> B.hGetContents (processStdout p) <*> B.hGetContents (processStderr p)

instance MimeRender PlainText HSBK where
    mimeRender Proxy = Lazy.encodeUtf8 . (<> "\n") . pShowNoColor
instance MimeRender PlainText Bool where
    mimeRender Proxy = Lazy.encodeUtf8 . (<> "\n") . pShowNoColor

subsumeFront :: Eff (eff : eff : effs) ~> Eff (eff : effs)
subsumeFront = subsume
