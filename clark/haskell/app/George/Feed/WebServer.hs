module George.Feed.WebServer (feed, Opts (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Data.Functor
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Typeable
import Data.Word
import Lifx.Lan (HSBK (..))
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App hiding (body)
import Okapi.Response
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import Util.Streamly.Okapi qualified as Okapi
import Util.Util
import Web.HttpApiData

data Opts = Opts
    { port :: Warp.Port
    , lifxMorningDelay :: NominalDiffTime
    , lifxMorningKelvin :: Word16
    }

feed :: Opts -> S.Stream IO [Event]
feed opts =
    S.catMaybes $
        Okapi.stream
            Okapi.Opts
                { warpSettings = Warp.setPort opts.port Warp.defaultSettings
                , routes = \act ->
                    [ lit "reset-error" . simpleGet $ f showT act $ send ResetError
                    , lit "exit" . param . simpleGet $ f showT act . send . Exit . maybe ExitSuccess ExitFailure
                    , lit "get-light-power" $ forEachRoom \(Proxy @r) ->
                        param . param . simpleGet . pairArgsE @r $
                            (f showT act . send . GetLightPower)
                    , lit "set-light-power" $ forEachRoom \(Proxy @r) ->
                        param . param . param . simpleGet . pairArgsE @r $
                            f showT act . send .: SetLightPower
                    , lit "get-light-colour" $ forEachRoom \(Proxy @r) ->
                        param . param . simpleGet . pairArgsE @r $
                            f showT act . send . GetLightColour
                    , lit "toggle-light" $ forEachRoom \(Proxy @r) ->
                        param . param . simpleGet . pairArgsE @r $
                            f showT act . toggleLight
                    , lit "set-light-colour" $ forEachRoom \(Proxy @r) ->
                        choice
                            [ param . param . param . param . param $
                                simpleGet $ pairArgs @r \light delay brightness kelvin ->
                                    f showT act $ send SetLightColourBK{lightBK = light, ..}
                            , param . param . param . param . param . param . param $
                                simpleGet $ pairArgs @r \light delay hue saturation brightness kelvin ->
                                    f showT act $ send SetLightColour{colour = HSBK{..}, ..}
                            ]
                    , lit "set-desk-power" . param . param . simpleGet $ f showT act . send .: SetDeskPower
                    , lit "send-email" . param . param $ simpleGet \subject body -> f showT act $ send SendEmail{..}
                    , lit "suspend-laptop" . simpleGet . f showT act $ send SuspendLaptop
                    , lit "set-other-led" . param . simpleGet $ f showT act . send . SetOtherLED
                    , lit "set-system-leds" . param . simpleGet $ f showT act . send . SetSystemLEDs
                    , lit "sleep-or-wake" . simpleGet . f showT act $
                        sleepOrWake opts.lifxMorningDelay opts.lifxMorningKelvin
                    ]
                }
            <&> \case
                Okapi.Event x -> Just [x]
                Okapi.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' (act :: Event -> IO ()) a ok _req = do
        m <- newEmptyMVar
        act $ ActionEvent (putMVar m) a
        ok noHeaders . (<> "\n") . show' <$> takeMVar m
    simpleGet = responder @200 @'[] @Text @Text . method GET id

-- we could easily inline these, but they just make the code prettier, to a surprising extent
pairArgs :: forall r c x. (RoomLightPair c -> x) -> SRoom r -> Light r c -> x
pairArgs f = f .: RoomLightPair
pairArgsE :: forall r x. (forall c. RoomLightPair c -> x) -> SRoom r -> Exists' (Light r) -> x
pairArgsE f r = withExists' $ pairArgs f r

-- TODO use explicit type arguments once available (GHC 9.10?) to simplify this
forEachRoom ::
    ( forall (r :: Room).
      -- TODO not all of these constraints are _always_ needed
      -- but these are the constraints which we want to ensure hold for _all_ rooms
      -- so, in lieu of a more direct way to assert this, this function is a handy way to ensure this is the case
      ( Typeable r
      , FromHttpApiData (Exists' (Light r))
      , FromHttpApiData (SRoom r)
      , FromHttpApiData (Light r KelvinOnly)
      , FromHttpApiData (Light r FullColours)
      ) =>
      Proxy r ->
      Node '[]
    ) ->
    Node '[]
forEachRoom f =
    choice
        [ f $ Proxy @LivingRoom
        , f $ Proxy @Bedroom
        , f $ Proxy @Office
        ]
