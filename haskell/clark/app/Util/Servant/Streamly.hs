-- TODO turn in to a library, replacing https://github.com/georgefst/georgefst-utils/blob/okapi-prerelease/streamly-okapi/Util/Streamly/Okapi.hs
module Util.Servant.Streamly where

import Control.Monad.IO.Class
import Data.Proxy
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp.Internal
import Servant
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as S

data Opts api a = Opts
    { warpSettings :: Warp.Settings
    , routes :: (a -> IO ()) -> Server api
    }

data Item a
    = Event a
    | WarpLog Wai.Request Status (Maybe Integer)

stream ::
    forall api m a.
    ( MonadIO m
    , HasServer api '[]
    ) =>
    Opts api a ->
    S.Stream m (Item a)
stream Opts{..} = S.morphInner liftIO $ S.fromEmitter \f ->
    let combinedLogger r s i = Warp.Internal.settingsLogger warpSettings r s i >> f (WarpLog r s i)
     in Warp.runSettings (Warp.setLogger combinedLogger warpSettings)
            . serve (Proxy @api)
            $ routes (f . Event)
