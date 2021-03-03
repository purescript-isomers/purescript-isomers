module Hybrid.App.Server where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Hybrid.Api.Server (Result) as Api.Server
import Hybrid.Api.Server (Result) as Server
import Hybrid.Api.Spec (ResponseCodec(..))
import Hybrid.App.Client.Router (RouterInterface)
import Hybrid.App.Renderer (Renderer)
import Hybrid.App.Spec (Raw(..)) as Spec
import Hybrid.HTTP.Exchange (fromResponse) as Exchange
import Node.HTTP (Response) as Node.HTTP
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Request.Duplex (Request, parse) as Request.Duplex
import Type.Prelude (class IsSymbol, SProxy)

-- | On the server side we want to work with this type: `Response (Lazy String /\ Lazy doc)`
-- | It gives as a way to respond to API calls but also to direct
-- | requests made directly by browser (when 'Accept: "text/html").
-- |
-- | We need a request because final rendering component
-- | takes it as a part of input.
render ∷
  ∀ doc req res.
  req →
  ResponseCodec res →
  Renderer req res doc →
  Server.Result res →
  Server.Result (Lazy String /\ doc)
render request (ResponseCodec codec) renderer (Left raw) = Left raw
render request (ResponseCodec codec) renderer (Right response) =
  let
    doc = renderer $ Exchange.fromResponse request response

    dump = defer \_ → codec.encode response
  in
    Right $ dump /\ doc


type Handler m req res
  = req → m (Server.Result res)

data RouterFolding handlers resCodecs renderers
  = RouterFolding { | handlers } { | resCodecs } { | renderers }

-- | This fold pattern maches over a request `Variant` to get
-- | renderer and handler.
instance routerFolding ::
  ( IsSymbol sym
  , Row.Cons sym (Handler m req res) handlers_ handlers
  , Row.Cons sym (ResponseCodec res) resCodecs_ resCodecs
  , Row.Cons sym (Renderer req res doc) renderers_ renderers
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resCodecs renderers)
    (SProxy sym)
    Unit
    req
    (m (Either Node.HTTP.Response (Lazy String /\ doc))) where
  foldingWithIndex (RouterFolding handlers resCodecs renderers) prop _ req =
    let
      renderer = Record.get prop renderers

      handler = Record.get prop handlers

      resCodec = Record.get prop resCodecs

    in
      do
        res ← handler req
        -- | On the server side we always have a response
        -- | which we can render.
        pure $ render req resCodec renderer res

data RoutingError
  = NotFound

router ∷
  ∀ doc handlers m renderers request resCodecs.
  Monad m ⇒
  HFoldlWithIndex (RouterFolding handlers resCodecs renderers) Unit (Variant request) (m (Api.Server.Result (Lazy String /\ doc))) ⇒
  Spec.Raw request resCodecs renderers →
  { | handlers } →
  Request.Duplex.Request →
  m (Either RoutingError (Api.Server.Result (Lazy String /\ doc)))
router spec@(Spec.Raw { codecs, renderers }) handlers = go
  where
  go raw = do
    case Request.Duplex.parse codecs.request raw of
      Right req → Right <$> hfoldlWithIndex (RouterFolding handlers codecs.response renderers) unit req
      Left err → pure $ Left NotFound


router' ∷
  ∀ doc handlers m renderers request resCodecs.
  Monad m ⇒
  HFoldlWithIndex (RouterFolding handlers resCodecs renderers) Unit (Variant request) (m (Api.Server.Result (Lazy String /\ Reader (RouterInterface request) doc))) ⇒
  Spec.Raw request resCodecs renderers →
  { | handlers } →
  Request.Duplex.Request →
  m (Either RoutingError (Api.Server.Result (Lazy String /\ Lazy doc)))
router' spec@(Spec.Raw { codecs, renderers }) handlers =
  let
    fakeWebRouter ∷ ∀ req. RouterInterface req
    fakeWebRouter =
      { navigate: const $ pure unit
      , redirect: const $ pure unit
      , submit: const $ pure unit
      }
  in \raw → do
    case Request.Duplex.parse codecs.request raw of
      Right req → do
        res ← hfoldlWithIndex (RouterFolding handlers codecs.response renderers) unit req
        let
          res' = map (\r → defer \_ → runReader r fakeWebRouter) <$> res
        pure (Right res')
      Left err → pure $ Left NotFound
