-- | I'm not very proud of the current implementation
-- |
module Isomers.Web.Client.Router where

import Prelude

import Control.Bind.Indexed (ibind)
import Control.Monad.Free.Trans (liftFreeT)
import Data.Either (Either(..), hush, note)
import Data.Identity (Identity(..))
import Data.Lazy (defer) as Lazy
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write) as Ref
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (ClientStep(..), Fetch, RequestBuildersStep)
import Isomers.Client (client, requestBuilders) as Client
import Isomers.Client.Fetch (HostInfo, fetch)
import Isomers.Client.Fetch (fetch) as Fetch
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.HTTP.ContentTypes (_html, _json) as ContentTypes
import Isomers.Request (Duplex) as Request
import Isomers.Request (ParsingError) as Isomers.Request
import Isomers.Request.Duplex (as) as Isomers.Request.Duplex
import Isomers.Request.Duplex (parse, print) as Request.Duplex
import Isomers.Request.Encodings (ServerRequest)
import Isomers.Response.Encodings (ClientResponse(..))
import Isomers.Spec (Spec(..))
import Isomers.Spec.Accept (ContentTypes)
import Isomers.Web.Client.Render (class FoldRender, ContractRequest, ExpandRequest, contractRequest, expandRequest, foldRender)
import Isomers.Web.Client.Render (contractRequest, expandRequest) as Isomers.Web.Client.Render
import Isomers.Web.Types (WebSpec(..))
import Network.HTTP.Types (hAccept)
import React.Basic (JSX)
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

serverRequest :: ∀ body. String → String → ServerRequest body
serverRequest accept path =
  { path
  , body: Right Nothing
  , headers: Lazy.defer \_ → Map.fromFoldable [ hAccept /\ accept ]
  , httpVersion: "HTTP/1.1"
  , method: "GET"
  }

apiServerRequest :: ∀ body. String → ServerRequest body
apiServerRequest = serverRequest (reflectSymbol $ ContentTypes._json)

webServerRequest :: ∀ body. String → ServerRequest body
webServerRequest = serverRequest (reflectSymbol $ ContentTypes._html)

parse ∷
  ∀ body ireq oreq rnd rndReq.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  Request.Duplex body (Variant ireq) (Variant oreq) → rnd → String → Aff (Maybe (Variant rndReq))
parse requestDuplex renderers path = do
  let
    -- | TODO: There is a bug probably because we are contracting "application/json"... why?
    parsePath = Request.Duplex.parse requestDuplex <<< webServerRequest
  map (contractRequest (Proxy ∷ Proxy (Variant oreq)) renderers <=< hush) <<< parsePath $ path

data AffRoute req res
  = Parsing Fetch String
  | Fetching Fetch req
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

type UrlString = String

type NavigationInterface client specReq oSpecReq webReq -- reqBuilders
  = { client ∷ client
    , navigate ∷ webReq → Effect Unit
    -- | String based parsing - should work only for
    , parse ∷
      { spec ∷ UrlString → Aff (Either Isomers.Request.ParsingError oSpecReq)
      , web ∷ UrlString → Aff (Either Isomers.Request.ParsingError webReq)
      }
    , print ∷
      { spec ∷ specReq → String
      , web ∷ webReq → String
      }
    -- , request ∷ reqBuilders
    , redirect ∷ webReq → Effect Unit
    -- | TODO: This is a quick hack to allow route changes without
    -- | fetch on router side. This does not trigger any rendering
    -- | of the component which is bad because we have inconsistent
    -- | route app state.
    -- | For sure an app should be able to change the URL and response
    -- | data state without a request on the router side and any unnecessary
    -- | parsing.
    , __replace ∷ webReq → Effect Unit
    -- | Because ajax calls result in automatic redirects handling
    -- | we probably should allow handling its results. like this.
    , __redirected ∷ String /\ ClientResponse → Effect Unit
    }

type NavigationInterface' client specReq webSpec = NavigationInterface client specReq specReq webSpec

parseSpecReq specReqDpl = Request.Duplex.parse specReqDpl <<< apiServerRequest

parseWebReq specReqDpl render = do
  let
    dpl = Isomers.Request.Duplex.as
      { print: Isomers.Web.Client.Render.expandRequest Proxy render
      , parse: note "Given URL is not a web route but a spec route." <<< Isomers.Web.Client.Render.contractRequest (Proxy ∷ Proxy (Variant _)) render
      , show: unsafeStringify
      }
      specReqDpl
  Request.Duplex.parse dpl <<< apiServerRequest

webRouter ::
  ∀ client clientRouter ireq oreq body requestBuilders rnd rndReq rndReqBuilders doc res.
  FoldRender (WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res) clientRouter (Variant rndReq) (Aff doc) =>
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  HFoldlWithIndex (ExpandRequest ireq) (Variant () → Variant ireq) rnd (Variant rndReq → Variant ireq) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq)) rndReqBuilders ⇒
  -- | client
  HFoldlWithIndex (RequestBuildersStep (Variant ireq) (Variant ireq)) {} (Proxy (Variant ireq)) requestBuilders ⇒
  HFoldlWithIndex (ClientStep (Variant ireq) res) {} requestBuilders client ⇒

  Defaults doc ->
  WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res ->
  HostInfo →
  -- (NavigationInterface (Variant ireq) (Variant rndReq) rndReqBuilders → clientRouter) →
  (NavigationInterface client (Variant ireq) (Variant oreq) (Variant rndReq) → clientRouter) →
  Aff
    ( Either
        String
        { component ∷ JSX
        , navigate ∷ Variant rndReq → Effect Unit
        , redirect ∷ Variant rndReq → Effect Unit
        , signal ∷ Signal doc
        }
    )
webRouter defaults webSpec@(WebSpec { spec: spec@(Spec { request: reqDpl, response }), render: HJust renderers }) hostInfo toClientRouter = do
  let
    defFetch = Fetch.fetch hostInfo

    render ∷ Fetch → Variant rndReq → clientRouter → Aff doc
    render = foldRender (Proxy ∷ Proxy clientRouter) webSpec

    parsePure ∷ String → Identity (AffRoute (Variant rndReq) doc)
    parsePure url = Identity (Parsing defFetch url)

    parseRoute url = parse reqDpl renderers url

    print ∷ AffRoute (Variant rndReq) doc → String
    print (Fetched req _) = unsafePrint reqDpl (expandRequest (Proxy ∷ Proxy (Variant ireq)) renderers req)

    print (Fetching _ req) = unsafePrint reqDpl (expandRequest (Proxy ∷ Proxy (Variant ireq)) renderers req)

    print (Parsing _ url) = url
  pushStateInterface ← liftEffect PushState.makeInterface
  liftEffect pushStateInterface.locationState >>= _.path
    >>> \initialRoute →
        parseRoute initialRoute
          >>= case _ of
              Nothing → pure $ Left $ "Invalid initial route: \"" <> initialRoute <> "\""
              Just initialRoute →
                liftEffect do
                  -- | I'm not able to render out of the box because...
                  -- | I need a router interface for rendering to work :-)
                  { signal, modify: modifySignal } ← Signal.create $ defaults.doc
                  reqRef ← liftEffect $ Ref.new $ Nothing -- print (Fetching initialRoute)
                  let
                    onRoute ∷
                      RouterInterface (AffRoute (Variant rndReq) doc) →
                      AffRoute (Variant rndReq) doc →
                      Router.Router (AffRoute (Variant rndReq) doc) Router.Transitioning Router.Resolved Unit
                    onRoute self route = do
                      let
                        -- I'm not sure if I should use `defFetch` in `client`
                        self' = toClientRouter
                          { client: Client.client defFetch reqDpl response
                          , navigate: self.navigate <<< Fetching defFetch
                          , parse:
                            { spec: parseSpecReq reqDpl
                            , web: parseWebReq reqDpl renderers
                            }
                          , print:
                            { spec: unsafePrint reqDpl
                            , web: \req → print (Fetching defFetch req)
                            }
                          , redirect: self.redirect <<< Fetching defFetch
                          , __replace: \req → do
                              Ref.write (Just $ print $ Fetching defFetch req) reqRef
                              self.redirect $ Fetching defFetch req
                          , __redirected: \(url /\ res) → self.navigate $ Parsing (const $ pure $ pure $ res) url
                          -- , request: webRequest webSpec
                          }
                        renderRouter fetch req = do
                          currReq ← liftEffect $ Ref.read reqRef
                          when (currReq /= (Just $ print $ Fetching fetch req)) do
                            doc ← liftAff $ render fetch req self'
                            liftEffect $ Ref.write (Just $ print $ Fetching fetch req) reqRef
                            liftFreeT $ Router.Override $ Fetched req doc
                      case route of
                        Parsing fetch url →
                          Router.Router do
                            liftAff (parseRoute url)
                              >>= case _ of
                                  Just req → renderRouter fetch req
                                  Nothing → pure unit
                        Fetching fetch req →
                          Router.Router $ renderRouter fetch req
                        Fetched _ _ → continue

                    onTransition ∷ Transition (AffRoute (Variant rndReq) doc) → Effect Unit
                    onTransition = case _ of
                      Router.Transitioning _ _ → pure unit
                      Router.Resolved _ (Parsing _ _) → pure unit
                      Router.Resolved _ (Fetching req _) → pure unit
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
                        , navigate: interface.navigate <<< Fetching defFetch
                        , redirect: interface.redirect <<< Fetching defFetch
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
  ∀ client ireq oreq body rnd rndReq requestBuilders responseDuplexes rndReqBuilders doc res.
  HFoldlWithIndex (ContractRequest oreq) (Variant oreq → Maybe (Variant ())) rnd (Variant oreq → Maybe (Variant rndReq)) ⇒
  HFoldlWithIndex (ExpandRequest ireq) (Variant () → Variant ireq) rnd (Variant rndReq → Variant ireq) ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq)) rndReqBuilders ⇒
  -- | client
  HFoldlWithIndex (RequestBuildersStep (Variant ireq) (Variant ireq)) {} (Proxy (Variant ireq)) requestBuilders ⇒
  HFoldlWithIndex (ClientStep (Variant ireq) res) {} requestBuilders client ⇒
  Fetch →
  doc →
  WebSpec body (HJust rnd) (Variant ireq) (Variant oreq) res ->
  NavigationInterface client (Variant ireq) (Variant oreq) (Variant rndReq) -- rndReqBuilders
fakeWebRouter fetch doc web@(WebSpec { spec: Spec spec@{ request: reqDpl, response: responseDpls }, render: HJust renderers }) =
  { client: Client.client fetch reqDpl responseDpls
  , navigate: const $ pure unit
  , parse:
    { spec: parseSpecReq reqDpl
    , web: parseWebReq reqDpl renderers
    }
  , print:
    { spec: unsafePrint reqDpl
    , web: unsafePrint reqDpl <<< expandRequest (Proxy ∷ Proxy (Variant ireq)) renderers
    }
  , redirect: const $ pure unit
  , __redirected: const $ pure unit
  , __replace: const $ pure unit
  }
