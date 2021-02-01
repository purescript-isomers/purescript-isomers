module WebRow.Hybrid.Router.Run where

import Prelude
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Functor.Variant (FProxy)
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Routing.Duplex (parse, print) as Routing.Duplex
import Routing.Duplex.Parser (RouteError)
import Run (Run)
import Run (lift) as Run
import Run.Except (EXCEPT, throwAt)
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row (type (+))
import WebRow.Hybrid.Router (Router(..))

-- | Let's keep our exchange types simple for now.
newtype Request content
  = Request
  { url ∷ String
  -- | Should we put here `Maybe`
  , content ∷ Maybe content
  }

newtype Response content
  = Response
  { statusCode ∷ Int
  , content ∷ Maybe content
  }

derive instance newtypeResponse ∷ Newtype (Response content) _

-- | XXX: Let's kiss encoding / decoding for now.
-- | * I don't want to tight this layer to purescript-response or purescript-polyform.
-- | * I don't want to care about character encoding and binary data transfer for now.
-- | * I'm going to add `Either` or `EXCEPT` into the stack later on.
newtype ResponseCodec a
  = ResponseCodec
  { decode ∷ Response String → Maybe a
  , encode ∷ a → Response String
  }

data FetchF req res a
  = FetchF
    (Lazy (Request String) /\ req)
    (Response String \/ res → a)

type FETCH req res
  = FProxy (FetchF req res)

derive instance functorFetchF ∷ Functor (FetchF req res)

liftFetchAt ∷
  ∀ t a r req res s.
  IsSymbol s ⇒
  Row.Cons s (FETCH req res) t r ⇒
  SProxy s →
  FetchF req res a →
  Run r a
liftFetchAt = Run.lift

type Handler res doc
  = Tuple (Lazy (Response String)) (Lazy (Maybe res)) → doc

handleWith ∷
  ∀ doc endpoint handlers_ handlers res responseCodecs_ responseCodecs.
  IsSymbol endpoint ⇒
  Row.Cons endpoint (Handler res doc) handlers_ handlers ⇒
  Row.Cons endpoint (ResponseCodec res) responseCodecs_ responseCodecs ⇒
  SProxy endpoint →
  Record responseCodecs →
  Record handlers →
  Response String \/ res →
  doc
handleWith endpoint response handlers =
  let
    codec = Record.get endpoint response

    handler = Record.get endpoint handlers
  in
    handle codec handler

handle ∷
  ∀ doc res.
  ResponseCodec res →
  Handler res doc →
  Response String \/ res →
  doc
handle (ResponseCodec codec) handler = case _ of
  Left raw →
    let
      raw' = defer \_ → raw

      res = defer \_ → codec.decode raw
    in
      handler (raw' /\ res)
  Right res →
    let
      raw = defer \_ → codec.encode res

      res' = defer \_ → Just res
    in
      handler (raw /\ res')

newtype RenderResponse handlers doc
  = RenderResponse { | handlers }

instance zipHandlers ::
  (IsSymbol sym, Row.Cons sym (Handler res doc) handlers_ handlers) =>
  MappingWithIndex
    (RenderResponse handlers doc)
    (SProxy sym)
    (ResponseCodec res)
    (Response String \/ res → doc) where
  mappingWithIndex (RenderResponse handlers) prop codec =
    let
      handler = Record.get prop handlers
    in
      handle codec handler

data Serve render
  = Serve (Lazy (Request String)) { | render }

instance serve ::
  ( IsSymbol sym
  , Row.Cons sym req reqRow_ reqRow
  , Row.Cons sym (FETCH req res) eff_ eff
  , Row.Cons sym (Response String \/ res → Run eff doc) render_ render
  ) =>
  FoldingWithIndex
    (Serve render)
    (SProxy sym)
    Unit
    req
    (Run eff doc) where
  foldingWithIndex (Serve rawReq render) prop _ req =
    let
      rnd = Record.get prop render
    in
      do
        res ← liftFetchAt prop $ FetchF (rawReq /\ req) identity
        rnd res

_routeNotFound = SProxy ∷ SProxy "routeNotFound"

type RouteNotFound eff
  = ( routeNotFound ∷ EXCEPT (RouteError /\ Request String) | eff )

route ∷
  ∀ doc eff handlers render req res.
  HMapWithIndex (RenderResponse handlers (Run (RouteNotFound + eff) doc)) { | res } { | render } ⇒
  HFoldlWithIndex (Serve render) Unit (Variant req) (Run (RouteNotFound + eff) doc) ⇒
  Router res req →
  { | handlers } →
  Request String \/ Variant req →
  Run (RouteNotFound + eff) doc
route router@(Router { request: requestDuplex, response }) handlers = go'
  where
  go' ∷ Request String \/ Variant req → Run (RouteNotFound + eff) doc
  go' = go $ render router handlers
    where
    render ∷
      HMapWithIndex (RenderResponse handlers (Run (RouteNotFound + eff) doc)) { | res } { | render } ⇒
      Router res req →
      { | handlers } →
      { | render }
    render (Router { response: responseCodec }) hs =
      hmapWithIndex
        (RenderResponse hs ∷ RenderResponse handlers (Run (RouteNotFound + eff) doc))
        responseCodec

  go ∷
    HFoldlWithIndex (Serve render) Unit (Variant req) (Run (RouteNotFound + eff) doc) ⇒
    -- RouteDuplex' (Variant req) →
    { | render } →
    Request String \/ Variant req →
    Run (RouteNotFound + eff) doc
  go render req = do
    raw /\ req' ← case req of
      Left raw@(Request { url }) → do
        case Routing.Duplex.parse requestDuplex url of
          Right r → pure $ (defer \_ → raw) /\ r
          Left err → throwAt _routeNotFound (err /\ raw)
      Right r → do
        let
          raw = defer \_ → Request { url: Routing.Duplex.print requestDuplex r, content: Nothing }
        pure $ raw /\ r
    hfoldlWithIndex (Serve raw render) unit req'
