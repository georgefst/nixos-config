{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

-- TODO fork, or take over, or even just rewrite fully?
-- https://github.com/morganthomas/servant-client-js/pull/6
-- https://github.com/morganthomas/servant-client-js/pull/7
import Servant.Client.JS

import API
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Bool
import Data.Coerce
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid.Extra
import Data.Ord (clamp)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import GHC.Generics (Generic)
import Language.Javascript.JSaddle.Wasm qualified as JSaddleWasm
import Miso hiding (Key)
import Miso.CSS qualified as CSS
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property
import Miso.String qualified as MS
import Optics
import Optics.State.Operators
import Servant.API
import Sol.API
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
    isGhci <- (== "<interactive>") <$> getProgName
    let baseUrl = getSolBaseUrl isGhci
    external <- refreshState False baseUrl
    (if isGhci then reload else startApp)
        defaultEvents
        (component Model{..} (updateModel baseUrl) (\() -> viewModel))
            { styles = mwhen isGhci [Href "assets/style.css" True]
            }
  where
    sliderPos = 0

data Model = Model
    { external :: ExternalState
    , sliderPos :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)

-- this is stuff that we don't control, but we try to reflect when we're effecting it, and occasionally just refresh
data ExternalState = ExternalState
    { hifiPower :: Bool
    , bulbs :: Map BulbGroup (Map BulbName (BulbInfo, BulbStatus))
    -- TODO
    -- , spotifyDevices :: [SpotifyDevice]
    }
    deriving stock (Eq, Ord, Show, Generic)

{- | Re-read everything we don't control.

@rescan@ asks the backend to go and look for bulbs again, rather than reusing the list it already
has. That's much slower (it's a broadcast, which always waits out its full collection window), so
it's reserved for the explicit rescan button - the backend drops bulbs which stop responding, and
this is the only way to get them back.
-}
refreshState :: Bool -> BaseUrl -> IO ExternalState
refreshState rescan baseUrl = do
    hifiPower <- f False $ runEndpoint baseUrl routes.getHifiPower
    bulbs <-
        f mempty . runEndpoint baseUrl $
            traverse (traverse \bi -> (bi,) <$> routes.getBulbStatus bi.group bi.name)
                . Map.fromListWith (<>)
                . map (\bi -> (bi.group, Map.singleton bi.name bi))
                =<< routes.getBulbs rescan
    -- spotifyDevices <- f [] $ runEndpoint baseUrl routes.getSpotifyDevices
    pure ExternalState{..}
  where
    f r = (either ((fmap \() -> r) . logClientError) pure =<<)

logClientError :: ClientError -> IO ()
logClientError = consoleError . ms . show

data Action
    = NoOp
    | Refresh
    | Rescan
    | UpdateExternalState ExternalState
    | UpdateExternalStatePartial (ExternalState -> ExternalState)
    | LogClientError ClientError
    | LogError MisoString
    | RunRequest (ClientM Action)

runRequest_ :: ClientM NoContent -> Action
runRequest_ = RunRequest . fmap \NoContent -> NoOp

-- TODO we do want some indication of when an action has completed
-- some can take a while, e.g. Spotify device transfer, or light scan
-- experiment with the `Sleep` action

updateModel :: BaseUrl -> Action -> Effect ROOT () Model Action
updateModel baseUrl = \case
    NoOp -> pure ()
    -- TODO maybe run this periodically? e.g. every five minutes as long as there's been no user interaction
    -- actually what you really want is for the backend to send this when it changes due to other sources...
    -- or even due to the same source - then we don't actually need our ad-hoc updates (`UpdateExternalStatePartial`)
    -- Miso does advertise some sort of SSE support - can we use that?
    Refresh -> do
        io $ UpdateExternalState <$> refreshState False baseUrl
        io_ $ either logClientError (\NoContent -> pure ()) =<< runEndpoint baseUrl routes.resetError
    -- unlike `Refresh`, this asks the backend to go looking for bulbs again, which is how a bulb
    -- that was dropped for being unresponsive gets back in to the list
    Rescan -> io $ UpdateExternalState <$> refreshState True baseUrl
    UpdateExternalState s -> #external .= s
    UpdateExternalStatePartial f -> #external %= f
    LogClientError e -> io_ $ consoleError $ ms $ show e
    LogError e -> io_ $ consoleError e
    RunRequest x -> io $ either LogClientError id <$> runEndpoint baseUrl x

viewModel :: Model -> View model Action
viewModel Model{external = ExternalState{..}} =
    div_
        []
        [ div_
            [id_ "system"]
            [ apiButton [id_ "power-off"] True "⏻" False $ runRequest_ routes.powerOff
            , apiButton [id_ "reboot"] True "↺" False $ runRequest_ routes.reboot
            , apiButton [id_ "refresh"] True "↻" False Refresh
            ]
        , div_
            [id_ "key-grid"]
            [ div_
                []
                [ apiButton [] True "↰" False $ runRequest_ $ routes.pressKey KeyEsc
                , apiButton [] True "↑" False $ runRequest_ $ routes.pressKey KeyUp
                , apiButton [] True "⇄" False . runRequest_ $ routes.pressKey KeyTab
                ]
            , div_
                []
                [ apiButton [] True "←" False $ runRequest_ $ routes.pressKey KeyLeft
                , apiButton [class_ "enter"] True "⊙" False . runRequest_ $ routes.pressKey KeyEnter
                , apiButton [] True "→" False $ runRequest_ $ routes.pressKey KeyRight
                ]
            , div_
                []
                [ apiButton [] True "␣" False $ runRequest_ $ routes.pressKey KeySpace
                , apiButton [] True "↓" False $ runRequest_ $ routes.pressKey KeyDown
                , apiButton [class_ "super"] True "⚙" False $ runRequest_ $ routes.pressKey KeySuper
                ]
            , div_
                []
                [ apiButton [class_ "tv"] True "📺" False $ runRequest_ routes.toggleTvPower --   TODO maybe the backend should try to retain a best guess of things live TV power statee
                , apiButton [class_ "hifi", onOrOff hifiPower] True (if hifiPower then "🔊" else "🔈") False . RunRequest $
                    let newPower = not hifiPower
                     in routes.setHifiPower newPower <&> \NoContent -> UpdateExternalStatePartial $ #hifiPower .~ newPower
                ]
            ]
        , div_
            [id_ "bulbs"]
            $ Map.toList bulbs <&> \(g, toList -> bs) ->
                div_
                    [class_ "group"]
                    [ div_ [class_ "title"] [text $ ms $ coerce g]
                    , div_ [class_ "widgets"] $
                        bs <&> \(bulb, status) ->
                            -- TODO there's a lot of indentation here - we should probably just split out some top-level defs
                            let
                                orEmpty = flip $ bool $ div_ [class_ "no-slider"] []
                                -- TODO see what happens when we just hit the API on every change instead?
                                -- otherwise WebRTC or Quic, along with hiding repeat events from our action log
                                -- and at that point, we can potentially just archive `lifx-manager` and remove it from NixOS install
                                bulbSlider ::
                                    forall a model.
                                    (Integral a, Bounded a, Read a, Show a) =>
                                    Lens' BulbColour a ->
                                    a ->
                                    View model Action
                                bulbSlider l step =
                                    input_
                                        [ type_ "range"
                                        , cssVars_ $
                                            -- intermediate "gradient stops" are crucial for hue, to avoid the interpolation being just red to red
                                            -- for the other three fields, this helps smooth over the inconsistencies in interpolation formulae
                                            -- i.e. the fact that the CSS uses "in hsl" since there's no "in hsbk"
                                            -- 9 stops is pretty arbitrary
                                            [0, 10 .. 100] <&> \x ->
                                                ( "colour-" <> ms x
                                                , let r = fromIntegral (maxBound @a) - fromIntegral (minBound @a)
                                                   in toRGB $ status.colour & l .~ (minBound + round ((fromIntegral @Int @Float x / 100) * r))
                                                )
                                        , min_ $ ms $ show $ minBound @a
                                        , max_ $ ms $ show $ maxBound @a
                                        , step_ $ ms $ show step
                                        , value_ $ ms $ show $ status.colour ^. l
                                        , onInput $ withValue $ UpdateExternalStatePartial . setLocal
                                        , onChange $ withValue \v ->
                                            RunRequest $
                                                routes.setBulbColour bulb.group bulb.name (status.colour & l .~ v) <&> \NoContent ->
                                                    UpdateExternalStatePartial $ setLocal v
                                        ]
                                  where
                                    setLocal v = adjustBulbStatus g bulb.name $ #colour % l .~ v
                             in
                                div_
                                    [ class_ "widget"
                                    , cssVar_ "colour" $ toRGB status.colour
                                    ]
                                    [ let
                                        newPower = not status.power
                                       in
                                        apiButton [onOrOff status.power] False (ms $ coerce bulb.name) False $
                                            RunRequest $
                                                routes.setBulbPower bulb.group bulb.name newPower <&> \NoContent ->
                                                    UpdateExternalStatePartial
                                                        . adjustBulbStatus g bulb.name
                                                        $ #power .~ newPower
                                    , div_
                                        [class_ "sliders"]
                                        $ mwhen
                                            -- TODO come up with principled way of deciding increment
                                            -- current divides range by 256, or 300 for Kelvin
                                            (any ((.hasColour) . fst) bs)
                                            [ orEmpty bulb.hasColour $ bulbSlider #hue 256
                                            , orEmpty bulb.hasColour $ bulbSlider #saturation 256
                                            ]
                                            <> [bulbSlider #brightness 256]
                                            <> mwhen
                                                (any ((.hasKelvin) . fst) bs)
                                                [ orEmpty bulb.hasKelvin $ bulbSlider #kelvin 25
                                                ]
                                    ]
                    ]
                    -- , div_
                    --     []
                    --     [ div_
                    --         [class_ "spotify-controls"]
                    --         $ spotifyDevices <&> \d ->
                    --             apiButton [] False (ms d.name) d.isActive $ runRequest_ $ routes.spotifyTransfer d.name
                    -- [
                    -- [ apiButton [] False "Refresh Devices" DoGetSpotifyDevices
                    -- , select_
                    --     [onChange (SetSpotifyDevice . fromMisoString)]
                    --     $ option_ [value_ ""] [text "-- select device --"]
                    --         : [ option_
                    --                 ( [value_ (ms d.name)]
                    --                     <> mwhen (spotifyDevice == Just d.name) [selected_ True]
                    --                 )
                    --                 [text $ ms $ d.name <> if d.isActive then " (active)" else ""]
                    --           | d <- spotifyDevices
                    --           ]
                    -- ,
                    --  apiButton [] False "Transfer" $ SpotifyTransfer _
                    -- ]
                    -- ]
        , div_
            [id_ "rescan"]
            [apiButton [] True "↻" False Rescan]
        ]
  where
    onOrOff = class_ . bool "off" "on"
    apiButton extraAttrs icon label disabled action =
        button_
            ( mwhen disabled [class_ "disabled"]
                <> [onClick action]
                <> extraAttrs
            )
            [applyWhen icon (div_ [class_ "icon"] . pure) $ text label]
    adjustBulbStatus g n f = #bulbs %~ Map.adjust (Map.adjust (_2 %~ f) n) g
    toRGB c =
        "rgb("
            <> ms (show $ channelRed * 100)
            <> ","
            <> ms (show $ channelGreen * 100)
            <> ","
            <> ms (show $ channelBlue * 100)
            <> ")"
      where
        RGB{..} = hsbkToRgb c
    withValue f = maybe NoOp f . readMaybe . fromMisoString

routes :: Routes (AsClientT ClientM)
routes = Servant.Client.JS.client $ Proxy @(NamedRoutes Routes)

-- TODO put this in its own module with type sig warnings disabled?
-- routes@Routes{..} = Servant.Client.JS.client $ Proxy @(NamedRoutes Routes)

-- TODO this MVar dance is a bit mental...
-- is there a more direct way to run a JSM action that returns a result?
-- also, I haven't even checked that it works yet - I think we really need a mock server
-- and actually given that we use Miso's `io` which is callback-based anyway, this could maybe be simpler
runEndpoint :: BaseUrl -> ClientM a -> IO (Either ClientError a)
runEndpoint baseUrl e = do
    m <- newEmptyMVar
    JSaddleWasm.run do
        r <- runClientM e $ ClientEnv baseUrl
        liftIO $ putMVar m r
    readMVar m

-- | The API base URL.
--
-- In dev (e.g. @sol-web-watch@, running under ghci-browser) the page is served
-- from a different origin than the mock server, so we target the mock server
-- at @localhost:8000@ explicitly (the mock server's CORS middleware handles
-- the cross-origin preflight).
--
-- In production the page and the API are served from the same origin by the
-- real @sol@ service on port 8000, so we use a *relative* URL: an empty
-- 'baseUrlHost' makes @servant-client-js@ emit a path-only fetch, which the
-- browser resolves against the page's own origin. Same-origin, so no CORS is
-- needed (and the production server has none).
--
-- @isGhci@ is captured once in 'main', before entering the JSM context, where
-- 'getProgName' reliably returns @"<interactive>"@ for ghci-browser. Inside
-- 'JSaddleWasm.run' the prog name is @"dyld.so"@ in both dev and prod, so it
-- can't be re-read here.
getSolBaseUrl :: Bool -> BaseUrl
getSolBaseUrl isGhci =
    if isGhci
        then BaseUrl Http "localhost" 8000 ""
        else BaseUrl Http "" 8000 ""

-- TODO copied almost exactly from `lifx-lan`
-- I think what we want is a pure sublibrary that re-exports this sort of thing
-- then `API.hs` here doesn't need it's own `BulbColour` and `Kelvin` types either
hsbkToRgb :: BulbColour -> RGB Float
hsbkToRgb BulbColour{..} =
    interpolateColour
        (fromIntegral saturation / maxWord16)
        c
        c'
  where
    c =
        hsv
            (360 * fromIntegral hue / maxWord16)
            (fromIntegral saturation / maxWord16)
            (fromIntegral brightness / maxWord16)
    c' =
        let t =
                (log (fromIntegral kelvin) - log minKelvin)
                    / log (maxKelvin / minKelvin)
         in clamp (0, 1)
                <$> RGB
                    { channelRed = 1
                    , channelGreen = t / 2 + 0.5
                    , channelBlue = t
                    }
    interpolateColour :: (Num a) => a -> RGB a -> RGB a -> RGB a
    interpolateColour r = liftA2 (\a b -> a * (r + b * (1 - r)))
    maxWord16 :: Float
    maxWord16 = fromIntegral $ maxBound @Word16
    minKelvin :: Float
    minKelvin = fromIntegral $ minBound @Kelvin
    maxKelvin :: Float
    maxKelvin = fromIntegral $ maxBound @Kelvin

-- TODO upstream?
cssVar_ :: MisoString -> MisoString -> Attribute action
cssVars_ :: [(MisoString, MisoString)] -> Attribute action
(cssVar_, cssVars_) = (curry $ style' . uncurry cssVar', style' . MS.intercalate ";" . map (uncurry cssVar'))
  where
    style' = textProp "style"
    cssVar' k v = "--" <> k <> " :" <> v

#ifdef wasi_HOST_OS
foreign export javascript "hs_start" main :: IO ()
#endif
