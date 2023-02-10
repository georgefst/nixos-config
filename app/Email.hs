module Email (send, Opts (..)) where

import Control.Lens ((&), (?~))
import Control.Monad.Catch (tryJust)
import Control.Monad.Except (MonadIO (..))
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException), HttpExceptionContent, Response)
import Network.Wreq (FormParam ((:=)), auth, basicAuth, defaults, postWith)
import Options.Generic (Text)

data Opts = Opts
    { mailgunKey :: Text
    , mailgunSandbox :: Text
    , subject :: Text
    , body :: Text
    }

send :: MonadIO m => Opts -> m (Either HttpExceptionContent (Response BSL.ByteString))
send Opts{..} =
    liftIO $ tryHttpException $ postWith postOpts url formParams
  where
    postOpts = defaults & auth ?~ basicAuth "api" (encodeUtf8 mailgunKey)
    url = "https://api.mailgun.net/v3/sandbox" <> T.unpack mailgunSandbox <> ".mailgun.org/messages"
    formParams =
        [ "from" := "Mailgun Sandbox <postmaster@sandbox" <> mailgunSandbox <> ".mailgun.org>"
        , "to" := ("George Thomas <georgefsthomas@gmail.com>" :: Text)
        , "subject" := subject
        , "text" := body
        ]
    tryHttpException = tryJust \case
        HttpExceptionRequest _ e -> Just e
        InvalidUrlException _ _ -> Nothing
