module Hybrid.App.Client.Router where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Free.Trans (liftFreeT)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (new, read, write) as Ref
import Heterogeneous.Folding (class HFoldlWithIndex)
import Hybrid.Api.Client.Fetch (exchange) as Client.Fetch
import Hybrid.App.Client.Render (RenderFolding)
import Hybrid.App.Client.Render (render) as Client.Render
import Hybrid.App.Spec (Raw(..)) as Spec
import Hybrid.HTTP (Exchange(..)) as HTTP
import Hybrid.HTTP (Exchange(..)) as Hybrid.HTTP
import Hybrid.HTTP.Exchange (FetchError)
import Hybrid.HTTP.Exchange (fromResponse) as Exchange
import Hybrid.HTTP.Response (Response)
import React.Basic (JSX)
import Request.Duplex (RequestDuplex')
import Request.Duplex (parse, print) as Request.Duplex
import Routing.PushState (makeInterface) as PushState
import Wire.React.Router (_Transition, continue, makeRouter)
import Wire.React.Router.Control (Command(..), Resolved, Router(..), Transition, Transitioning) as Router
import Wire.Signal (Signal)
import Wire.Signal (create) as Signal

type RouterInterface req
  = { navigate :: Variant req -> Effect Unit
    , print ∷ Variant req → String
    , redirect :: Variant req -> Effect Unit
    , submit :: Variant req -> Effect Unit
    }

-- | TODO: This is unsafe now (we can have POST requests which we
-- | dump into plain strings).
-- | We should probably split POST and GET rows so this can be
-- | a safe method.
print ∷ ∀ req. RequestDuplex' req → Hybrid.HTTP.Exchange req String → String
print requestDuplex (Hybrid.HTTP.Exchange request res) =
  let
    { path } = Request.Duplex.print requestDuplex request
  in
    path

router ∷
  ∀ doc req res rnd.
  HFoldlWithIndex (RenderFolding (RouterInterface req) req res rnd) Unit (Variant req) (Maybe (Either FetchError (Response String)) → doc) ⇒
  Spec.Raw req res rnd →
  Maybe (Either FetchError (Response String)) →
  Effect { component ∷ JSX, signal ∷ Signal doc, interface ∷ RouterInterface req }
router spec@(Spec.Raw { codecs }) initialResponse = do
  initiaRef ← Ref.new initialResponse

  let
    parse ∷ ∀ a. String → Either _ (Hybrid.HTTP.Exchange (Variant req) a)
    parse path =
      Request.Duplex.parse codecs.request { method: "GET", headers: mempty, path, body: "" }
        <#> \request →
            Hybrid.HTTP.Exchange request Nothing


  pushStateInterface ← PushState.makeInterface
  currState ← pushStateInterface.locationState
  case parse currState.path of
    Left err → throwError (error $ "Error parsing initial path: " <> show currState.path)
    Right (Hybrid.HTTP.Exchange req _) → do
      let
        initial = Hybrid.HTTP.Exchange req Nothing

      { signal, modify } ← Signal.create initial
      let
        render = Client.Render.render spec

        onRoute ∷ Hybrid.HTTP.Exchange (Variant req) String → Router.Router (Hybrid.HTTP.Exchange (Variant req) String) Router.Transitioning Router.Resolved Unit
        onRoute (Hybrid.HTTP.Exchange request Nothing) =
          Router.Router do
            p ← liftEffect $ do
              p ← Ref.read initiaRef
              Ref.write Nothing initiaRef
              pure p

            case p of
              Just res → liftFreeT $ Router.Override $ HTTP.Exchange request (Just res)
              otherwise → do
                ex ← liftAff $ Client.Fetch.exchange codecs.request request
                liftFreeT $ Router.Override ex

        onRoute (Hybrid.HTTP.Exchange request (Just resp)) = continue

        onTransition ∷ Router.Transition (Hybrid.HTTP.Exchange (Variant req) String) → Effect Unit
        onTransition transition = modify (const $ view _Transition transition)
      interface@{ component, navigate, redirect, submit } ←
        makeRouter
          pushStateInterface
          { parse
          , print: print codecs.request
          , onRoute
          , onTransition
          }
      let
        redirect' ∷ Variant req → Effect Unit
        redirect' route = redirect (Hybrid.HTTP.Exchange route Nothing)

        navigate' ∷ Variant req → Effect Unit
        navigate' route = navigate (Hybrid.HTTP.Exchange route Nothing)

        submit' ∷ Variant req → Effect Unit
        submit' route = submit (Hybrid.HTTP.Exchange route Nothing)

        print' ∷ Variant req → String
        print' route = print codecs.request (Hybrid.HTTP.Exchange route Nothing)

        interface' = { navigate: navigate', print: print', redirect: redirect', submit: submit' }

        signal' = signal <#> \exchange →
          let
            jsx = render (interface' /\ exchange)
            x = do
              traceM "Debugging jsx:"
              traceM jsx
              Nothing
          in
            jsx
      pure { component, signal: signal', interface: interface' }
