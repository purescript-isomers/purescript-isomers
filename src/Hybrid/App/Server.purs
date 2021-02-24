module Hybrid.App.Server where

import Prelude

import Data.Either (Either(..))
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class MappingWithIndex)
import Hybrid.Api.Spec (ResponseCodec(..))
import Hybrid.App.Spec (Raw(..)) as Spec
import Hybrid.App.Renderer (Renderer)
import Hybrid.Response (Response)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Request.Duplex (Request, parse) as Request.Duplex
import Type.Prelude (class IsSymbol, SProxy)

type Handler m req res
  = req → m (Response res)

data Route handlers resCodecs renderers
  = Route { | handlers } { | resCodecs } { | renderers }

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
  (Response res → Response (Lazy String /\ Lazy doc))
render req (ResponseCodec codec) renderer res =
  res
    <#> \content →
        let
          doc = defer \_ → renderer req (Just (Right res))

          dump = defer \_ → codec.encode content
        in
          (dump /\ doc)


-- | Iterate over response codecs and build a single rendering
-- | function by applying selected renderer to the response.
newtype RenderResponse renderers
  = RenderResponse { | renderers }

instance renderResponse ::
  (IsSymbol sym, Row.Cons sym (Renderer req res doc) renderers_ renderers) =>
  MappingWithIndex
    (RenderResponse renderers)
    (SProxy sym)
    (ResponseCodec res)
    (req → Response res → Response (Lazy String /\ Lazy doc)) where
  mappingWithIndex (RenderResponse renderers) prop codec =
    let
      renderer = Record.get prop renderers
    in
      \request → render request codec renderer


instance routeFold ::
  ( IsSymbol sym
  , Row.Cons sym (Handler m req res) handlers_ handlers
  , Row.Cons sym (ResponseCodec res) resCodecs_ resCodecs
  , Row.Cons sym (Renderer req res doc) renderers_ renderers
  , Monad m
  ) =>
  FoldingWithIndex
    (Route handlers resCodecs renderers)
    (SProxy sym)
    Unit
    req
    (m (Response (Lazy String /\ Lazy doc))) where
  foldingWithIndex (Route handlers resCodecs renderers) prop _ req =
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

route ∷
  ∀ doc handlers m renderers request resCodecs.
  Monad m ⇒
  HFoldlWithIndex (Route handlers resCodecs renderers) Unit (Variant request) (m (Response (Lazy String /\ Lazy doc))) ⇒
  Spec.Raw request resCodecs renderers →
  { | handlers } →
  Request.Duplex.Request →
  m (Either RoutingError (Response (Lazy String /\ Lazy doc)))
route spec@(Spec.Raw { codecs, renderers }) handlers = go
  where
  go raw = do
    case Request.Duplex.parse codecs.request raw of
      Right req → Right <$> hfoldlWithIndex (Route handlers codecs.response renderers) unit req
      Left err → pure $ Left NotFound
