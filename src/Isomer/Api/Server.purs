module Isomer.Api.Server where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomer.Api.Spec (Spec(..))
import Isomer.HTTP.Request (Data(..)) as Request
import Isomer.HTTP.Response.Duplex (Duplex) as Response
import Isomer.HTTP.Response.Duplex (encode) as Response.Duplex
import Isomer.HTTP.Response.Node (Interface) as Response.Node
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Request.Duplex (Request, parse) as Request.Duplex

type Handler aff req res
  = req → aff res

data RouterFolding handlers resDuplexes
  = RouterFolding { | handlers } { | resDuplexes }

-- | This is the bottom of the request path.
-- | We pass data to the handler.
instance routerFoldingFun ::
  ( IsSymbol sym
  , Row.Cons sym (Handler aff req res) handlers_ handlers
  , Row.Cons sym (Response.Duplex aff res res) resDuplexes_ resDuplexes
  , Monad aff
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resDuplexes)
    (SProxy sym)
    Unit
    (Request.Data req)
    (Response.Node.Interface aff → aff Unit) where
  foldingWithIndex (RouterFolding handlers resDuplexes) prop _ (Request.Data req) =
    let
      handler = Record.get prop handlers

      resDpl = Record.get prop resDuplexes
    in
      \nodeInterface → do
        res ← handler req
        Response.Duplex.encode nodeInterface resDpl res
else instance routerFoldingNewtypeRec ::
  ( IsSymbol sym
  , Row.Cons sym (f { | subhandlers }) handlers_ handlers
  , Row.Cons sym (f { | subcodecs }) resDuplexes_ resDuplexes
  , Newtype (f { | subhandlers }) { | subhandlers }
  , Newtype (f { | subcodecs }) { | subcodecs }
  , Newtype (f (Variant req)) (Variant req)
  , HFoldlWithIndex (RouterFolding subhandlers subcodecs) (Variant req) Unit (Response.Node.Interface m → m Unit)
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resDuplexes)
    (SProxy sym)
    Unit
    (f (Variant req))
    (Response.Node.Interface m → m Unit) where
  foldingWithIndex (RouterFolding handlers resDuplexes) prop _ req =
    let
      subhandlers = unwrap (Record.get prop handlers)

      subcodecs = unwrap (Record.get prop resDuplexes)
    in
      hfoldlWithIndex (RouterFolding subhandlers subcodecs) (unwrap req) unit
else instance routerFoldingRec ::
  ( IsSymbol sym
  , Row.Cons sym { | subhandlers } handlers_ handlers
  , Row.Cons sym { | subcodecs } resCodecs_ resCodecs
  , HFoldlWithIndex (RouterFolding subhandlers subcodecs) (Variant req) Unit (Response.Node.Interface m → m Unit)
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resCodecs)
    (SProxy sym)
    Unit
    (Variant req)
    (Response.Node.Interface m → m Unit) where
  foldingWithIndex (RouterFolding handlers resCodecs) prop _ req =
    let
      subhandlers = Record.get prop handlers

      subcodecs = Record.get prop resCodecs
    in
      hfoldlWithIndex (RouterFolding subhandlers subcodecs) req unit

data RoutingError
  = NotFound

router ∷
  ∀ handlers m request resCodecs.
  Monad m ⇒
  HFoldlWithIndex (RouterFolding handlers resCodecs) Unit (Variant request) (m String) ⇒
  Spec (Variant request) { | resCodecs } →
  { | handlers } →
  Request.Duplex.Request →
  m (Either RoutingError String)
router spec@(Spec { request, response }) handlers = go
  where
  go raw = do
    case Request.Duplex.parse request raw of
      Right req → Right <$> hfoldlWithIndex (RouterFolding handlers response) unit req
      Left err → pure $ Left NotFound

