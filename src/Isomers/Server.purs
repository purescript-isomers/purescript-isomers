module Isomers.Server
  ( module Exports
  , router
  , RouterStep(..)
  , ServerResponseWrapper(..)
  , RoutingError(..)
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff, liftAff)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Request.Duplex (parse) as Request.Duplex
import Isomers.Request.Encodings (ServerRequest) as Request.Encodings
import Isomers.Response (Duplex, print) as Response
import Isomers.Response.Encodings (ServerResponse) as Response.Encodings
import Isomers.Server.Handler (Handler)
import Isomers.Server.Handler (unifyMonad, Handler) as Exports
import Isomers.Spec (Spec(..))
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (Proxy)

-- | TODO: Drop wrapper when on purs-0.14.0
-- | because it is just an for quite a large
-- | `ServerResponse` raw `Record`.
newtype ServerResponseWrapper = ServerResponseWrapper Response.Encodings.ServerResponse

derive instance newtypeServerResponseWrapper ∷ Newtype ServerResponseWrapper _

data RouterStep handlers resDuplexes
  = RouterStep { | handlers } { | resDuplexes }

instance routerFoldingRec ::
  ( IsSymbol sym
  , Row.Cons sym { | subhandlers } handlers_ handlers
  , Row.Cons sym { | subcodecs } resCodecs_ resCodecs
  , HFoldlWithIndex (RouterStep subhandlers subcodecs) Unit (Variant req) (m ServerResponseWrapper)
  ) =>
  FoldingWithIndex
    (RouterStep handlers resCodecs)
    (Proxy sym)
    Unit
    (Variant req)
    (m ServerResponseWrapper) where
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
  , HFoldlWithIndex (RouterStep subhandlers subcodecs) Unit (Variant req) (m ServerResponseWrapper)
  ) =>
  FoldingWithIndex
    (RouterStep handlers resDuplexes)
    (Proxy sym)
    Unit
    (f (Variant req))
    (m ServerResponseWrapper) where
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
  , Row.Cons sym (Handler m req res) handlers_ handlers
  , Row.Cons sym (Response.Duplex ct res res') resDuplexes_ resDuplexes
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterStep handlers resDuplexes)
    (Proxy sym)
    Unit
    req
    (m ServerResponseWrapper) where
  foldingWithIndex (RouterStep handlers resDuplexes) prop _ req = do
    let
      handler = Record.get prop handlers

      resDpl = Record.get prop resDuplexes
    res ← handler req
    pure $ ServerResponseWrapper (Response.print resDpl res)

data RoutingError = NotFound

router ∷
  ∀ body handlers ireq oreq m resCodecs.
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit oreq (m ServerResponseWrapper) ⇒
  MonadAff m ⇒
  Spec body ireq oreq { | resCodecs } →
  { | handlers } →
  Request.Encodings.ServerRequest body →
  m (Either RoutingError Response.Encodings.ServerResponse)
router spec@(Spec { request, response }) handlers = do
  let
    handle = hfoldlWithIndex (RouterStep handlers response) unit

  \raw → liftAff (Request.Duplex.parse request raw) >>= case _ of
    Right req → (Right <<< un ServerResponseWrapper) <$> handle req
    Left err → pure $ Left NotFound

