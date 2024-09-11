{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Freer
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either.Extra
import Data.Kind
import Data.List.Extra
import Data.Proxy
import Data.Time (NominalDiffTime)
import Network.Socket
import Options.Generic
import RawFilePath
import System.Exit

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

subsumeFront :: Eff (eff : eff : effs) ~> Eff (eff : effs)
subsumeFront = subsume

-- TODO there must be libraries for this sort of thing
data Exists c t where -- A simple wrapper in lieu of first-class existential types.
    Exists :: (c a) => t a -> Exists c t
withExists :: (forall a. (c a) => t a -> b) -> Exists c t -> b
withExists f (Exists a) = f a
class NullConstraint a
instance NullConstraint a
pattern Exists' :: () => t a -> Exists' t
pattern Exists' a = Exists a
type Exists' = Exists NullConstraint
withExists' :: (forall a. t a -> b) -> Exists' t -> b
withExists' f (Exists a) = f a
class ToProxyList (c :: Type -> Constraint) (ts :: [Type]) where
    toProxyList :: [Exists c Proxy]
instance ToProxyList c '[] where
    toProxyList = []
instance (c t, ToProxyList c ts) => ToProxyList c (t : ts) where
    toProxyList = Exists @c (Proxy @t) : toProxyList @c @ts

-- this is a nicer "modern Haskell" interface than I've seen elsewhere for catching multiple exception types
-- we keep the second version around because it gives slightly more flexibility in obscure cases
catchMany ::
    forall (ts :: [Type]) m a.
    (MonadCatch m, ToProxyList Exception ts) =>
    (forall e. (Exception e) => e -> m a) ->
    m a ->
    m a
catchMany = catchMany' $ toProxyList @Exception @ts
catchMany' ::
    forall m a.
    (MonadCatch m) =>
    [Exists Exception Proxy] ->
    (forall e. (Exception e) => e -> m a) ->
    m a ->
    m a
catchMany' ps h = flip catches . fmap (withExists \(_ :: Proxy e) -> Handler @_ @_ @e h) $ ps

newtype IP = IP {unIP :: HostAddress}
    deriving stock (Generic)
    deriving anyclass (ParseRecord, ParseField, ParseFields)
instance Show IP where
    show (IP x) = intercalate "." $ map show [a, b, c, d]
      where
        (a, b, c, d) = hostAddressToTuple x
instance Read IP where
    readsPrec _ s = case map read $ splitOn "." s of
        [a, b, c, d] -> pure $ (,"") $ IP $ tupleToHostAddress (a, b, c, d)
        _ -> []

deriving anyclass instance ParseField PortNumber
deriving anyclass instance ParseFields PortNumber
instance ParseRecord PortNumber where
    parseRecord = fmap getOnly parseRecord

deriving anyclass instance ParseField NominalDiffTime
deriving anyclass instance ParseFields NominalDiffTime
instance ParseRecord NominalDiffTime where
    parseRecord = fmap getOnly parseRecord
