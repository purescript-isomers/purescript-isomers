-- | I'm not very proud of the current implementation
-- |
module Isomers.Web.Client.Router where

import Prelude

import Control.Bind.Indexed (ibind)
import Control.Monad.Free.Trans (liftFreeT)
import Data.Either (Either(..), hush)
import Data.Identity (Identity(..))
import Data.Lazy (defer) as Lazy
import Data.Map (empty, fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write) as Ref
import Foreign.Object (fromHomogeneous) as Object
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (RequestBuildersStep)
import Isomers.Client (requestBuilders) as Client
import Isomers.Client.Fetch (HostInfo)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.Request (Duplex) as Request
import Isomers.Request.Duplex (parse, print) as Request.Duplex
import Isomers.Spec (Spec(..))
import Isomers.Web.Client.Render (class FoldRender, ContractRequest, ExpandRequest, contractRequest, expandRequest, foldRender)
import Isomers.Web.Types (WebSpec(..))
import Network.HTTP.Types (hAccept)
import React.Basic (JSX)
import React.Basic.Hooks (component)
import Routing.PushState (makeInterface) as PushState
import Type.Prelude (Proxy(..))
import Wire.React.Router (Transition, continue, makeRouter)
import Wire.React.Router.Control (Command(..), Router(..), Transition(..), Transitioning, Resolved) as Router
import Wire.Signal (Signal)
import Wire.Signal (create) as Signal

-- | TODO: this is somewhat unsafe because we don't preserve any HTTP semantics
-- | on the requsts level at the moment. We would like to probably
-- | have something like this here:
-- |
-- | HTTPRequest (method ∷ Method ("GET ::: SList), headers ∷ HNil) req ⇒
-- |
-- | so we can be sure that a give request can be safely dumped into just URL
unsafePrint ∷ ∀ body ireq oreq. Request.Duplex body ireq oreq → ireq → String
unsafePrint requestDuplex request = _.path $ Request.Duplex.print requestDuplex request

parse ∷
  ∀ body ireq oreq rnd rndReq.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  Request.Duplex body (Variant ireq) (Variant oreq) → rnd → String → Aff (Maybe (Variant rndReq))
parse requestDuplex renderers path = do
  let
    -- | TODO: There is a bug probably because we are contracting "application/json"... why?
    parsePath p =
      Request.Duplex.parse requestDuplex
        { path
        , body: Right Nothing
        , headers: Lazy.defer \_ → Map.fromFoldable [ hAccept /\ "text/html" ]
        , httpVersion: "HTTP/1.1"
        , method: "GET"
        }
  map (contractRequest (Proxy ∷ Proxy (Variant oreq)) renderers <=< hush) <<< parsePath $ path

data AffRoute req res
  = Parsing String
  | Fetching req
  | Fetched req res

webRequest ∷
  ∀ body rnd ireq oreq rndReq rndReqBuilders res.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq)) rndReqBuilders ⇒
  WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res →
  rndReqBuilders
webRequest webSpec@(WebSpec { spec: spec@(Spec { request: reqDpl, response }), render: HJust render }) = Client.requestBuilders (Proxy ∷ Proxy (Variant rndReq))

type Defaults doc
  = { doc ∷ doc }

type RouterInterface req
  = { navigate ∷ req → Effect Unit
    , redirect ∷ req → Effect Unit
    , component ∷ JSX
    }

type NavigationInterface req reqBuilders
  = { navigate ∷ req → Effect Unit
    , request ∷ reqBuilders
    , redirect ∷ req → Effect Unit
    }

webRouter ::
  ∀ ireq oreq body rnd rndReq rndReqBuilders doc res.
  FoldRender (WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res) (NavigationInterface (Variant rndReq) rndReqBuilders) (Variant rndReq) (Aff doc) =>
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  HFoldlWithIndex (ExpandRequest ireq) (Variant () → Variant ireq) rnd (Variant rndReq → Variant ireq) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq)) rndReqBuilders ⇒
  Defaults doc ->
  WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res ->
  HostInfo →
  Aff
    ( Either
        String
        { component ∷ JSX
        , navigate ∷ Variant rndReq → Effect Unit
        , redirect ∷ Variant rndReq → Effect Unit
        , signal ∷ Signal doc
        }
    )
webRouter defaults webSpec@(WebSpec { spec: spec@(Spec { request: reqDpl, response }), render: HJust renderers }) hostInfo = do
  let
    render ∷ Variant rndReq → NavigationInterface (Variant rndReq) rndReqBuilders → Aff doc
    render = foldRender (Proxy ∷ Proxy (NavigationInterface (Variant rndReq) rndReqBuilders)) webSpec hostInfo

    parsePure ∷ String → Identity (AffRoute (Variant rndReq) doc)
    parsePure url = Identity (Parsing url)

    parseRoute url = parse reqDpl renderers url

    print ∷ AffRoute (Variant rndReq) doc → String
    print (Fetched req _) = unsafePrint reqDpl (expandRequest (Proxy ∷ Proxy (Variant ireq)) renderers req)

    print (Fetching req) = unsafePrint reqDpl (expandRequest (Proxy ∷ Proxy (Variant ireq)) renderers req)

    print (Parsing url) = url
  pushStateInterface ← liftEffect PushState.makeInterface
  liftEffect pushStateInterface.locationState >>= _.path >>> \initialRoute → parseRoute initialRoute
    >>= case _ of
        Nothing → pure $ Left $ "Invalid initial route: \"" <> initialRoute <> "\""
        Just initialRoute →
          liftEffect do
            -- | I'm not able to render out of the box because...
            -- | I need a router interface for rendering to work :-)
            { signal, modify: modifySignal } ← Signal.create $ defaults.doc
            let
              onRoute ∷
                RouterInterface (AffRoute (Variant rndReq) doc) →
                AffRoute (Variant rndReq) doc →
                Router.Router (AffRoute (Variant rndReq) doc) Router.Transitioning Router.Resolved Unit
              onRoute self route = do
                let
                  self' =
                    { navigate: self.navigate <<< Fetching
                    , redirect: self.redirect <<< Fetching
                    , request: webRequest webSpec
                    }
                case route of
                  Parsing url →
                    Router.Router do
                      liftAff (parseRoute url)
                        >>= case _ of
                            Just req → do
                              doc ← liftAff $ render req self'
                              liftFreeT $ Router.Override $ Fetched req doc
                            Nothing → pure unit
                  Fetching req →
                    Router.Router do
                      doc ← liftAff $ render req self'
                      liftFreeT $ Router.Override $ Fetched req doc
                  Fetched _ _ → continue

              onTransition ∷ Transition (AffRoute (Variant rndReq) doc) → Effect Unit
              onTransition = case _ of
                Router.Transitioning _ _ → pure unit
                Router.Resolved _ (Parsing _) → pure unit
                Router.Resolved _ (Fetching req) → pure unit
                Router.Resolved _ (Fetched req doc) → do
                  modifySignal (const $ doc)
            (interface ∷ (RouterInterface (AffRoute (Variant rndReq) doc))) ←
              makeRouter'
                pushStateInterface
                { onRoute
                , onTransition
                , parse: parsePure
                , print
                }
            pure
              $ Right
                  { component: interface.component
                  , navigate: interface.navigate <<< Fetching
                  , redirect: interface.redirect <<< Fetching
                  , signal
                  }
  where
  -- | `makeRouter` version which passes `self` reference to the `onRoute` function
  makeRouter' interface opts = do
    ref ← Ref.new { component: mempty, navigate: const $ pure unit, redirect: const $ pure unit }
    let
      onRoute = \route → do
        router ← Router.Router $ liftEffect $ Ref.read ref
        opts.onRoute router route
        where
        bind = ibind

      opts' = opts { onRoute = onRoute }
    router ← makeRouter interface opts'
    Ref.write router ref
    pure router

-- | We pass here `WebSpec` only to somehow generate `Variant rndReq` type.
fakeWebRouter ∷
  ∀ ireq oreq body rnd rndReq rndReqBuilders doc res.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq)) rndReqBuilders ⇒
  doc →
  WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res ->
  { navigate ∷ Variant rndReq → Effect Unit
  , redirect ∷ Variant rndReq → Effect Unit
  , request ∷ rndReqBuilders
  }
fakeWebRouter doc web =
  { navigate: const $ pure unit
  , redirect: const $ pure unit
  , request: webRequest web
  }
