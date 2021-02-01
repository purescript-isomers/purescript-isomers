module WebRow.Hybrid.Router.Run where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer) as Control.Lazy
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Functor.Variant (FProxy(..), VariantF)
import Data.Identity (Identity(..))
import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Validation.Semigroup (V(..))
import Data.Variant (Unvariant(..), Variant, case_, inj, on)
import Data.Variant (Variant, unvariant)
import Data.Variant (class Contractable, contract, expand, inj) as Variant
import Data.Variant.Internal (VariantRep(..))
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mappingWithIndex)
import Polyform.Batteries.Json.Duals ((:=))
import Polyform.Batteries.Json.Duals (Base, Pure, arrayOf, boolean, int, null, object, string) as Json.Duals
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Validator.Dual (lmapDual)
import Polyform.Validator.Dual.Pure (Dual) as Validator.Dual.Pure
import Polyform.Validator.Dual.Pure (runValidator) as Dual.Pure
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Cons, class Union) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import Prim.Symbol (class Append) as Symbol
import Record (get, insert, union) as Record
import Record.Builder (Builder, build) as Record.Builder
import Record.Extra (class MapRecord, mapRecord)
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Record.Unsafe (unsafeSet)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', end, path)
import Routing.Duplex (RouteDuplex(..), RouteDuplex', int, parse, prefix, print, string) as Routing.Duplex
import Routing.Duplex (RouteDuplex(..), RouteDuplex', segment)
import Routing.Duplex (record) as Dual
import Routing.Duplex.Generic.Variant (Updater(..), modify, update) as Generic.Variant
import Routing.Duplex.Generic.Variant (class VariantParser, class VariantPrinter, variant, variant')
import Routing.Duplex.Parser (RouteError(..), RouteParser(..), RouteResult(..)) as Duplex.Parser
import Routing.Duplex.Parser (RouteError, RouteParser)
import Routing.Duplex.Printer (RoutePrinter(..))
import Run (Run(..))
import Run (case_, lift, onMatch) as Run
import Run.Except (EXCEPT, throwAt)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (App, Id) as Eval.Function
import Type.Eval.Function (Id) as Eval.Functionl
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map) as Eval.Functor
import Type.Eval.RowList (FromRow, ToRow) as Eval.RowList
import Type.Eval.Tuple (Curry, Snd, Uncurry) as Eval.Tuple
import Type.Prelude (RLProxy(..), SProxy(..), reflectSymbol)
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row (type (+), RProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Hybrid.Router (Router(..))

-- | Let's keep our exchange types simple for now.
newtype Request content = Request
  { url ∷ String
  -- | Should we put here `Maybe`
  , content ∷ Maybe content
  }

newtype Response content = Response
  { statusCode ∷ Int
  , content ∷ Maybe content
  }

derive instance newtypeResponse ∷ Newtype (Response content) _

-- | XXX: Let's kiss encoding / decoding for now.
-- | * I don't want to tight this layer to purescript-response or purescript-polyform.
-- | * I don't want to care about character encoding and binary data transfer for now.
-- | * I'm going to add `Either` or `EXCEPT` into the stack later on.
newtype ResponseCodec a = ResponseCodec
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
  ∀ t a r req res route s.
  IsSymbol s ⇒
  Row.Cons s (FETCH req res) t r ⇒
  SProxy s →
  FetchF req res a →
  Run r a
liftFetchAt = Run.lift

type Handler res doc = Tuple (Lazy (Response String)) (Lazy (Maybe res)) → doc

handleWith ∷ ∀ doc endpoint handlers_ handlers res responseCodecs_ responseCodecs.
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

handle ∷ ∀ doc res.
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

newtype RenderResponse handlers doc = RenderResponse { | handlers }

instance zipHandlers ::
  (IsSymbol sym, Row.Cons sym (Handler res doc) handlers_ handlers) =>
  MappingWithIndex
    (RenderResponse handlers doc)
    (SProxy sym)
    (ResponseCodec res)
    (Response String \/ res → doc)
  where
  mappingWithIndex (RenderResponse handlers) prop codec =
    let
      handler = Record.get prop handlers
    in
      handle codec handler

data Serve render = Serve (Lazy (Request String)) { | render }

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
    (Run eff doc)
  where
    foldingWithIndex (Serve rawReq render) prop _ req =
      let
        rnd = Record.get prop render
      in do
        res ← liftFetchAt prop $ FetchF (rawReq /\ req) identity
        rnd res

_routeNotFound = SProxy ∷ SProxy "routeNotFound"

type RouteNotFound eff = (routeNotFound ∷ EXCEPT (RouteError /\ Request String) | eff)


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

  render ∷
    HMapWithIndex (RenderResponse handlers (Run (RouteNotFound + eff) doc)) { | res } { | render } ⇒
    Router res req →
    { | handlers } →
    { | render }
  render (Router { request: requestDuplex, response }) handlers =
    hmapWithIndex (RenderResponse handlers ∷ RenderResponse handlers (Run (RouteNotFound + eff) doc)) response

  r = render router handlers

  go' ∷ Request String \/ Variant req → Run (RouteNotFound + eff) doc
  go' = go requestDuplex r

  go ∷
    HFoldlWithIndex (Serve render) Unit (Variant req) (Run (RouteNotFound + eff) doc) ⇒
    RouteDuplex' (Variant req) →
    { | render } →
    Request String \/ Variant req →
    Run (RouteNotFound + eff) doc
  go requestDuplex render req = do
    raw /\ req' ← case req of
      Left raw@(Request { url }) → do
        case Routing.Duplex.parse requestDuplex url of
          Right r → pure $ (defer \_ →  raw) /\ r
          Left err → throwAt _routeNotFound (err /\ raw)
      Right r → do
        let
          raw = defer \_ → Request { url: Routing.Duplex.print requestDuplex r, content: Nothing }
        pure $ raw /\ r
    hfoldlWithIndex (Serve raw render) unit req'

