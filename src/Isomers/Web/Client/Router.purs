module Isomers.Web.Client.Router where

import Prelude

import Data.Either (Either(..), hush, note)
import Data.Lazy (defer) as Lazy
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write) as Ref
import Halogen.Subscription (Emitter)
import Halogen.Subscription (create, notify) as Subscription
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Client (ClientStep, Fetch, RequestBuildersStep)
import Isomers.Client (client, requestBuilders) as Client
import Isomers.Client.Fetch (HostInfo)
import Isomers.Client.Fetch (fetch) as Fetch
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..))
import Isomers.Contrib.Web.Router.Driver.PushState (makeDriverAff)
import Isomers.HTTP.ContentTypes (_html, _json) as ContentTypes
import Isomers.Request (Duplex, ParsingError) as Request
import Isomers.Request.Duplex (as) as Isomers.Request.Duplex
import Isomers.Request.Duplex (parse, print) as Request.Duplex
import Isomers.Request.Encodings (ServerRequest)
import Isomers.Response.Encodings (ClientResponse)
import Isomers.Spec (Spec(..))
import Isomers.Web.Client.Render
  ( class FoldRender
  , ContractRequest
  , ExpandRequest
  , contractRequest
  , expandRequest
  , foldRender
  )
import Isomers.Web.Client.Render (contractRequest, expandRequest) as Isomers.Web.Client.Render
import Isomers.Web.Types (WebSpec(..))
import JS.Unsafe.Stringify (unsafeStringify)
import Network.HTTP.Types (hAccept)
import Type.Prelude (Proxy(..))
import Web.Router (Router, RouterState(..), continue, makeRouter) as Web.Router

-- import Wire.React.Router.Control (Command(..), Router(..), Transition(..), Transitioning, Resolved) as Router
-- import Wire.Signal (Signal)
-- import Wire.Signal (create) as Signal

-- | This printing is somewhat unsafe because we turn a given URL into just
-- | URL string (without headers and using method to "GET").
-- | Because we don't preserve any HTTP semantics on the request level at the moment
-- | it is hard to guard against invalid encoding.
-- | We would like to probably have something like this here:
-- | ```
-- | HTTPRequest (method ∷ Method ("GET" ::: SNil), headers ∷ HNil) req ⇒
-- | ```
-- | so we can be sure that a give request can be safely dumped into just an URL
-- |
unsafePrint :: forall ireq oreq. Request.Duplex ireq oreq -> ireq -> String
unsafePrint requestDuplex request = _.path $ Request.Duplex.print requestDuplex request

parseWebURL
  :: forall ireq oreq rnd rndReq
   . HFoldlWithIndex (ContractRequest oreq) (Variant oreq -> Maybe (Variant ())) rnd
       (Variant oreq -> Maybe (Variant rndReq))
  => Request.Duplex (Variant ireq) (Variant oreq)
  -> rnd
  -> String
  -> Aff (Maybe (Variant rndReq))
parseWebURL requestDuplex renderers path = do
  let
    -- | TODO: There is probably a bug here because we are contracting "application/json"... why?
    parsePath = Request.Duplex.parse requestDuplex <<< webServerRequest
  map (contractRequest (Proxy :: Proxy (Variant oreq)) renderers <=< hush) <<< parsePath $ path

serverRequest :: String -> String -> ServerRequest
serverRequest accept path =
  { path
  , body: Nothing
  , headers: Lazy.defer \_ -> Map.fromFoldable [ hAccept /\ accept ]
  , httpVersion: "HTTP/1.1"
  , method: "GET"
  }

apiServerRequest :: String -> ServerRequest
apiServerRequest = serverRequest (reflectSymbol $ ContentTypes._json)

webServerRequest :: String -> ServerRequest
webServerRequest = serverRequest (reflectSymbol $ ContentTypes._html)

webRequest
  :: forall rnd ireq oreq rndReq rndReqBuilders res
   . HFoldlWithIndex (ContractRequest oreq) (Variant oreq -> Maybe (Variant ())) rnd
       (Variant oreq -> Maybe (Variant rndReq))
  => HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq))
       { | rndReqBuilders }
  => WebSpec (HJust rnd) (Variant ireq) (Variant oreq) res
  -> { | rndReqBuilders }
webRequest _ =
  Client.requestBuilders (Proxy :: Proxy (Variant rndReq))

type Defaults doc = { doc :: doc }

type UrlString = String

type NavigationInterface client inSpecReq outSpecReq webReq rndReqBuilders =
  { client :: client
  , navigate :: webReq -> Effect Unit
  , parse ::
      { spec :: UrlString -> Aff (Either Request.ParsingError outSpecReq)
      , web :: UrlString -> Aff (Either Request.ParsingError webReq)
      }
  , print ::
      { spec :: inSpecReq -> String
      , web :: webReq -> String
      }
  , request :: rndReqBuilders
  , redirect :: webReq -> Effect Unit
  -- | TODO: This is a quick hack to allow route changes without
  -- | fetch on the router side. This does not trigger any rendering
  -- | of the component which is bad because we have inconsistent
  -- | route app state.
  -- | I think that an app should be able to change the URL and the response
  -- | data state without triggering a request on the router side.
  , __replace :: webReq -> Effect Unit
  -- | Because ajax redirects are automatically handled by the browser
  -- | we probably should allow handling its results like this.
  , __redirected :: String /\ ClientResponse -> Effect Unit
  }

type NavigationInterface' client specReq webSpec reqBuilders = NavigationInterface client specReq specReq webSpec
  reqBuilders

parseSpecReq :: forall t166 t167. Request.Duplex t166 t167 -> String -> Aff (Either Request.ParsingError t167)
parseSpecReq specReqDpl = Request.Duplex.parse specReqDpl <<< apiServerRequest

-- parseWebReq :: forall t179 t181 t191 t192 t193 t206. HFoldlWithIndex (ExpandRequest t179) (Variant () -> Variant t179) t192 (Variant t181 -> Variant t179) => HFoldlWithIndex (ContractRequest t191) (Variant t191 -> Maybe (Variant ())) t192 (Variant t191 -> Maybe (Variant t193)) => Request.Duplex t206 (Variant t179) (Variant t191) -> t192 -> String -> Aff (Either Request.ParsingError (Variant t193))
parseWebReq specReqDpl render = do
  let
    dpl =
      Isomers.Request.Duplex.as
        { print: Isomers.Web.Client.Render.expandRequest Proxy render
        , parse: note "Given URL is not a web route but a spec route." <<< Isomers.Web.Client.Render.contractRequest
            (Proxy :: Proxy (Variant _
))
            render
        , show: unsafeStringify
        }
        specReqDpl
  Request.Duplex.parse dpl <<< apiServerRequest

webRouter
  :: forall client clientRouter ireq oreq requestBuilders rnd rndReq rndReqBuilders doc res
   . FoldRender (WebSpec (HJust rnd) (Variant ireq) (Variant oreq) res) clientRouter (Variant rndReq) (Aff doc)
  => HFoldlWithIndex (ContractRequest oreq) (Variant oreq -> Maybe (Variant ())) rnd
       (Variant oreq -> Maybe (Variant rndReq))
  => HFoldlWithIndex (ExpandRequest ireq) (Variant () -> Variant ireq) rnd (Variant rndReq -> Variant ireq)
  => HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq))
       { | rndReqBuilders }
  =>
  -- | client
  HFoldlWithIndex (RequestBuildersStep (Variant ireq) (Variant ireq)) {} (Proxy (Variant ireq)) { | requestBuilders }
  => HFoldlWithIndex (ClientStep (Variant ireq) res) {} { | requestBuilders } client
  => Defaults doc
  -> WebSpec (HJust rnd) (Variant ireq) (Variant oreq) res
  -> HostInfo
  ->
  -- (NavigationInterface (Variant ireq) (Variant rndReq) rndReqBuilders → clientRouter) →
  (NavigationInterface client (Variant ireq) (Variant oreq) (Variant rndReq) { | rndReqBuilders } -> clientRouter)
  -> Effect
       ( Either
           String
           { initialize :: Effect (Effect Unit)
           , navigate :: Variant rndReq -> Effect Unit
           , redirect :: Variant rndReq -> Effect Unit
           , emitter :: Emitter doc
           }
       )
webRouter
  _ -- defaults
  webSpec@(WebSpec { spec: Spec { request: reqDpl, response }, render: HJust renderers })
  hostInfo
  toClientRouter = do
  let
    -- | TODO: fetch should be parameterized
    defFetch = Fetch.fetch hostInfo

    render :: Fetch -> Variant rndReq -> clientRouter -> Aff doc
    render = foldRender (Proxy :: Proxy clientRouter) webSpec

    printWebRequest req = unsafePrint reqDpl (expandRequest (Proxy :: Proxy (Variant ireq)) renderers req)
    printSpecRequest req = unsafePrint reqDpl req

  driver <- makeDriverAff (parseWebURL reqDpl renderers) printWebRequest

  reqRef <- Ref.new $ Nothing
  { emitter, listener } <- Subscription.create

  let
    handleState
      :: Web.Router.Router (Variant rndReq)
      -> Web.Router.RouterState (Variant rndReq)
      -> Effect Unit
    handleState self = case _ of
      Web.Router.Transitioning _ _ -> pure unit
      Web.Router.Resolved _ req -> do
        currReq <- Ref.read reqRef
        when (currReq /= (Just $ printWebRequest req)) do
          let
            self' =
              toClientRouter
                { client: Client.client defFetch reqDpl response
                , navigate: self.navigate
                , parse:
                    { spec: parseSpecReq reqDpl
                    , web: parseWebReq reqDpl renderers
                    }
                , print:
                    { spec: printSpecRequest
                    , web: printWebRequest
                    }
                , request: webRequest webSpec
                , redirect: self.redirect
                , __replace:
                    \req -> do
                      Ref.write (Just $ printWebRequest $ req) reqRef
                      self.redirect $ req
                , __redirected: \(url /\ res) -> do
                    -- | TODO: I'm not sure what should go into `url`
                    -- self.navigate $ Parsing (const $ pure $ pure $ res) url
                    pure unit
                -- , request: webRequest webSpec
                }
          Ref.write (Just $ printWebRequest req) reqRef
          launchAff_ do
            doc <- render defFetch req self'
            liftEffect $ Subscription.notify listener doc

  (interface :: (Web.Router.Router (Variant rndReq))) <-
    makeRouter'
      driver
      handleState
  pure
    $ Right
        { initialize: interface.initialize
        , navigate: interface.navigate
        , redirect: interface.redirect
        , emitter
        }
  where
  -- | `makeRouter` version which passes `self` reference to the `onRoute` function
  makeRouter' driver handleState = do
    ref <- Ref.new
      { initialize: pure (pure unit)
      , navigate: const $ pure unit
      , redirect: const $ pure unit
      }
    let
      handleState' st = do
        router <- Ref.read ref
        handleState router st

    router <- Web.Router.makeRouter
      (\_ _ -> Web.Router.continue)
      handleState'
      driver
    Ref.write router ref
    pure router

-- | We pass here `WebSpec` only to somehow generate `Variant rndReq` type.
fakeWebRouter
  :: forall client ireq oreq rnd rndReq requestBuilders rndReqBuilders doc res
   . HFoldlWithIndex (ContractRequest oreq) (Variant oreq -> Maybe (Variant ())) rnd
       (Variant oreq -> Maybe (Variant rndReq))
  => HFoldlWithIndex (ExpandRequest ireq) (Variant () -> Variant ireq) rnd (Variant rndReq -> Variant ireq)
  =>
  -- HFoldlWithIndex (Client.RequestBuildersStep ireq ireq) {} (Proxy ireq) { | requestBuilders } ⇒
  HFoldlWithIndex (RequestBuildersStep (Variant rndReq) (Variant rndReq)) {} (Proxy (Variant rndReq))
    { | rndReqBuilders }
  =>
  -- | client
  HFoldlWithIndex (RequestBuildersStep (Variant ireq) (Variant ireq)) {} (Proxy (Variant ireq)) { | requestBuilders }
  => HFoldlWithIndex (ClientStep (Variant ireq) res) {} { | requestBuilders } client
  => Fetch
  -> doc
  -> WebSpec (HJust rnd) (Variant ireq) (Variant oreq) res
  -> NavigationInterface client (Variant ireq) (Variant oreq) (Variant rndReq) { | rndReqBuilders }
fakeWebRouter
  fetch
  doc
  web@(WebSpec { spec: Spec { request: reqDpl, response: responseDpls }, render: HJust renderers }) =
  { client: Client.client fetch reqDpl responseDpls
  , navigate: const $ pure unit
  , parse:
      { spec: parseSpecReq reqDpl
      , web: parseWebReq reqDpl renderers
      }
  , print:
      { spec: unsafePrint reqDpl
      , web: unsafePrint reqDpl <<< expandRequest (Proxy :: Proxy (Variant ireq)) renderers
      }
  , redirect: const $ pure unit
  , request: webRequest web
  , __redirected: const $ pure unit
  , __replace: const $ pure unit
  }
