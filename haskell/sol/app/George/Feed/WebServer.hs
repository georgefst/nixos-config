module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util.Servant.Curl

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.Client (BaseUrl (..), Scheme (Http))
import Streamly.Data.Stream.Prelude qualified as S
import Util.Servant.Streamly qualified as Servant
import Util.Util

data Opts = Opts
    { port :: Warp.Port
    , curlDocsCallback :: Text -> IO ()
    }

type R = Get '[PlainText] Text
data Routes mode = Routes
    { resetError :: mode :- "tmp" :> R
    , getCurrentLight :: mode :- "light" :> R
    , spotifyTransfer :: mode :- "spotify" :> Capture "device" Text :> R
    }
    deriving (Generic)

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Servant.stream @(NamedRoutes Routes)
            Servant.Opts
                { warpSettings =
                    Warp.setBeforeMainLoop
                        (opts.curlDocsCallback $ curlDocs opts.port)
                        $ Warp.setPort opts.port Warp.defaultSettings
                , routes = \act ->
                    Routes
                        { resetError = f showT act $ send ResetError
                        , getCurrentLight = f showT act $ send . GetLightName =<< send GetCurrentLight
                        , spotifyTransfer = f showT act . (send . flip SpotifyTransfer True <=< send . SpotifyGetDevice)
                        }
                }
            <&> \case
                Servant.Event x -> Just [x]
                Servant.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' (act :: Event -> IO ()) a = liftIO do
        m <- newEmptyMVar
        act $ ActionEvent (putMVar m) a
        (<> "\n") . show' <$> takeMVar m

curlDocs :: Int -> Text
curlDocs port =
    T.intercalate "\n" $
        zipWith
            (\v es -> T.unlines $ v : es)
            (curlFunctions host api)
            (curlExamples host api)
  where
    api = Proxy @(NamedRoutes Routes)
    host =
        BaseUrl
            { baseUrlScheme = Http
            , baseUrlHost = "sol"
            , baseUrlPort = port
            , baseUrlPath = ""
            }
