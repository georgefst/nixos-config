-- inspired by https://docs.servant.dev/en/latest/cookbook/curl-mock/CurlMock.html
-- but perhaps not aiming for enough generality to be worth turning in to a library
-- anyway, servant-foreign has a major limitation, in that all content types have to be JSON
-- and we're only working around that by forking and making plain text the one supported type instead
-- (and hardcoding `text/plain` here)
-- https://github.com/haskell-servant/servant/issues/290#issuecomment-4929533128
module Util.Servant.Curl (
    Curl,
    Var,
    Examples,
    typeForExamples,
    curlFunctions,
    curlExamples,
) where

import Data.List.Extra
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Word
import Network.HTTP.Types
import Optics
import Servant
import Servant.Client (BaseUrl, showBaseUrl)
import Servant.Foreign
import Util.Util

type data Curl

data Var = Var

instance HasForeignType Curl Var a where
    typeFor _ _ _ = Var

newtype Examples = Examples [Text]
unExamples :: Examples -> [Text]
unExamples (Examples xs) = xs
typeForExamples :: (ToHttpApiData a) => [a] -> Proxy Curl -> Proxy Examples -> Proxy a -> Examples
typeForExamples xs _ _ _ = Examples $ map toUrlPiece xs
instance HasForeignType Curl Examples Text where
    typeFor = typeForExamples ["abc", "t"]
instance HasForeignType Curl Examples Bool where
    typeFor = typeForExamples enumerate
instance HasForeignType Curl Examples NominalDiffTime where
    typeFor = typeForExamples [3.14]
instance HasForeignType Curl Examples Int where
    typeFor = typeForExamples [0, 12]
instance HasForeignType Curl Examples Word16 where
    typeFor = typeForExamples [minBound, maxBound]

generateEndpoint :: BaseUrl -> Req ftype -> [Text] -> Maybe Text -> Text
generateEndpoint host req segs maybeBody =
    T.intercalate " " $
        ["curl"]
            <> ( let m = req ^. lensVL reqMethod
                  in mwhen
                        (m /= methodGet)
                        [ "-X"
                        , decodeUtf8 m
                        ]
               )
            <> ( maybeBody & foldMap @Maybe \b ->
                    [ "-d"
                    , "'" <> b <> "'"
                    , "-H 'Content-Type: text/plain'"
                    ]
               )
            <> [ T.pack (showBaseUrl host) <> "/" <> T.intercalate "/" segs
               ]

curlFunctions ::
    ( HasForeign Curl Var api
    , GenerateList Var (Foreign Var api)
    ) =>
    BaseUrl -> Proxy api -> [Text]
curlFunctions host api =
    listFromAPI (Proxy @Curl) Proxy api <&> \req ->
        generateEndpoint host req (seg <$> getSegs req) (body <$> getBody req)
  where
    seg = \case
        Static p -> unPathSegment p
        Cap arg -> "$" <> unPathSegment (arg ^. lensVL argName)
    body Var = "$body"
curlExamples ::
    ( HasForeign Curl Examples api
    , GenerateList Examples (Foreign Examples api)
    ) =>
    BaseUrl -> Proxy api -> [[Text]]
curlExamples host api =
    listFromAPI (Proxy @Curl) Proxy api <&> \req ->
        generateEndpoint host req <$> traverse seg (getSegs req) <*> traverse body (getBody req)
  where
    seg = \case
        Static p -> [unPathSegment p]
        Cap arg -> unExamples (arg ^. lensVL argType)
    body = unExamples

getSegs :: Req ftype -> [SegmentType ftype]
getSegs req = map unSegment $ req ^. lensVL (reqUrl . path)
getBody :: Req ftype -> Maybe ftype
getBody req = req ^. lensVL reqBody
