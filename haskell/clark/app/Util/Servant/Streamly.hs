-- TODO turn in to a library, replacing https://github.com/georgefst/georgefst-utils/blob/okapi-prerelease/streamly-okapi/Util/Streamly/Okapi.hs
module Util.Servant.Streamly where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Proxy
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp.Internal
import Servant
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S

data Opts api = Opts
    { warpSettings :: Warp.Settings
    , routes :: ServerT api (ExceptT ServerError IO)
    }

-- TODO hmm, not even returning an event at all feels quite odd... how does this even work?
-- original idea was to use Cont or something in the Servant monad stack
data Item
    = WarpLog Wai.Request Status (Maybe Integer)
    -- | Event

stream ::
    forall api m.
    ( MonadIO m
    , HasServer api '[]
    ) =>
    Opts api ->
    S.Stream m Item
stream Opts{..} = S.morphInner liftIO $ S.fromEmitter \f ->
    let combinedLogger r s i = Warp.Internal.settingsLogger warpSettings r s i >> f (WarpLog r s i)
     in Warp.runSettings (Warp.setLogger combinedLogger warpSettings) $
            serveWithContextT (Proxy @api) EmptyContext Handler routes
