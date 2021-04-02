module Isomers.Server where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Request (ServerRequest)
import Isomers.Request (parse) as Request
import Isomers.Response (Duplex, print) as Response
import Isomers.Response.Duplex.Encodings (ServerResponse)
import Isomers.Spec (Spec(..))
import Prim.Row (class Cons) as Row
import Record (get) as Record

type Handler req res = req → Aff res

-- TODO: Drop wrapper when on purs-0.14.0
newtype ServerResponseWrapper = ServerResponseWrapper ServerResponse

derive instance newtypeServerResponseWrapper ∷ Newtype ServerResponseWrapper _

data RouterStep handlers resDuplexes
  = RouterStep { | handlers } { | resDuplexes }

instance routerFoldingRec ::
  ( IsSymbol sym
  , Row.Cons sym { | subhandlers } handlers_ handlers
  , Row.Cons sym { | subcodecs } resCodecs_ resCodecs
  , HFoldlWithIndex (RouterStep subhandlers subcodecs) Unit (Variant req) (Aff ServerResponseWrapper)
  ) =>
  FoldingWithIndex
    (RouterStep handlers resCodecs)
    (SProxy sym)
    Unit
    (Variant req)
    (Aff ServerResponseWrapper) where
  foldingWithIndex (RouterStep handlers resCodecs) prop _ req =
    let
      subhandlers = Record.get prop handlers

      subcodecs = Record.get prop resCodecs
    in
      hfoldlWithIndex (RouterStep subhandlers subcodecs) unit req
else instance routerFoldingNewtypeRec ::
  ( IsSymbol sym
  , Row.Cons sym (f { | subhandlers }) handlers_ handlers
  , Row.Cons sym (f { | subcodecs }) resDuplexes_ resDuplexes
  , Newtype (f (Variant req)) (Variant req)
  , Newtype (f { | subhandlers }) { | subhandlers }
  , Newtype (f { | subcodecs }) { | subcodecs }
  , HFoldlWithIndex (RouterStep subhandlers subcodecs) Unit (Variant req) (Aff ServerResponseWrapper)
  ) =>
  FoldingWithIndex
    (RouterStep handlers resDuplexes)
    (SProxy sym)
    Unit
    (f (Variant req))
    (Aff ServerResponseWrapper) where
  foldingWithIndex (RouterStep handlers resDuplexes) prop _ req =
    let
      subhandlers = unwrap (Record.get prop handlers)

      subcodecs = unwrap (Record.get prop resDuplexes)
    in
      hfoldlWithIndex (RouterStep subhandlers subcodecs) unit (unwrap req)
-- | This is the bottom of the request path.
-- | We pass data to the handler.
else instance routerFoldingFun ::
  ( IsSymbol sym
  , Row.Cons sym (Handler req res) handlers_ handlers
  , Row.Cons sym (Response.Duplex ct res res) resDuplexes_ resDuplexes
  ) =>
  FoldingWithIndex
    (RouterStep handlers resDuplexes)
    (SProxy sym)
    Unit
    req
    (Aff ServerResponseWrapper) where
  foldingWithIndex (RouterStep handlers resDuplexes) prop _ req = do
    let
      handler = Record.get prop handlers

      resDpl = Record.get prop resDuplexes
    res ← handler req
    pure $ ServerResponseWrapper (Response.print resDpl res)

data RoutingError = NotFound

router ∷
  ∀ body handlers request resCodecs.
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit (Variant request) (Aff ServerResponseWrapper) ⇒
  Spec body {} (Variant request) { | resCodecs } →
  { | handlers } →
  ServerRequest body →
  Aff (Either RoutingError ServerResponse)
router spec@(Spec { request, response }) handlers = do
  let
    handle = hfoldlWithIndex (RouterStep handlers response) unit

  \raw → Request.parse request raw {} >>= case _ of
    Right req → (Right <<< un ServerResponseWrapper) <$> handle req
    Left err → pure $ Left NotFound

