module Hybrid.Api.Server where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Hybrid.Api.Spec (ResponseCodecs(..), Spec(..))
import Hybrid.HTTP.Request (Data(..)) as Request
import Hybrid.HTTP.Response.Codec (Codec(..)) as Response
import Hybrid.HTTP.Response.Node (Interface) as Response.Node
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Request.Duplex (Request, parse) as Request.Duplex

-- | TODO: Move to `VariantF` response representation
-- | which will be tide to a given endpoint:
-- | ```purescript
-- | newtype OkF payload = OkF headers payload
-- |
-- | type Ok c res = (ok ∷ FProxy OkF | res)
-- |
-- | newtype RedirectF payload = RedirectF { location ∷ String, permament ∷ Boolean }
-- |
-- | type Redirect res = (redirect ∷ FProxy RedirecF | res)
-- |
-- | newtype AttachmentF payload = AttachmentF { content ∷ ArrayBuffer, fileName ∷ String }
-- |
-- | type Attachment res = (attachment ∷ AttachmentF | res)
-- | ```
type Handler aff req res
  = req → aff res

data RouterFolding handlers resCodecs
  = RouterFolding { | handlers } { | resCodecs }

-- | This folding can be run on given request `Variant` to get
-- | renderer and handler.
instance routerFolding ::
  ( IsSymbol sym
  , Row.Cons sym (Handler aff req res) handlers_ handlers
  , Row.Cons sym (Response.Codec aff res res) resCodecs_ resCodecs
  , Monad aff
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resCodecs)
    (SProxy sym)
    Unit
    (Request.Data req)
    (Response.Node.Interface aff → aff Unit) where
  foldingWithIndex (RouterFolding handlers resCodecs) prop _ (Request.Data req) =
    let
      handler = Record.get prop handlers

      Response.Codec encode _ = Record.get prop resCodecs
    in
      \nodeInterface → do
        res ← handler req
        encode nodeInterface res
else instance routerFoldingNewtypeRec ::
  ( IsSymbol sym
  , Row.Cons sym (f { | subhandlers }) handlers_ handlers
  , Row.Cons sym (f { | subcodecs }) resCodecs_ resCodecs
  , Newtype (f { | subhandlers }) { | subhandlers }
  , Newtype (f { | subcodecs }) { | subcodecs }
  , Newtype (f (Variant req)) (Variant req)
  , HFoldlWithIndex (RouterFolding subhandlers subcodecs) (Variant req) Unit (m String)
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resCodecs)
    (SProxy sym)
    Unit
    (f (Variant req))
    (m String) where
  foldingWithIndex (RouterFolding handlers resCodecs) prop _ req =
    let
      subhandlers = unwrap (Record.get prop handlers)

      subcodecs = unwrap (Record.get prop resCodecs)
    in
      hfoldlWithIndex (RouterFolding subhandlers subcodecs) (unwrap req) unit
else instance routerFoldingRec ::
  ( IsSymbol sym
  , Row.Cons sym { | subhandlers } handlers_ handlers
  , Row.Cons sym { | subcodecs } resCodecs_ resCodecs
  , HFoldlWithIndex (RouterFolding subhandlers subcodecs) (Variant req) Unit (m String)
  , Monad m
  ) =>
  FoldingWithIndex
    (RouterFolding handlers resCodecs)
    (SProxy sym)
    Unit
    (Variant req)
    (m String) where
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
router spec@(Spec { request, response: ResponseCodecs response }) handlers = go
  where
  go raw = do
    case Request.Duplex.parse request raw of
      Right req → Right <$> hfoldlWithIndex (RouterFolding handlers response) unit req
      Left err → pure $ Left NotFound

-- -- -- | Currently handler context is just a router printer function.
-- -- data ArgMapping ctx = ArgMapping ctx --
-- -- 
-- -- instance handlerContextMapping ∷
-- --   (TypeEquals a (ctx → h)) ⇒
-- --   Mapping (ArgMapping ctx) a h where
-- --   mapping (ArgMapping ctx) f = (to f) ctx
-- -- 
-- -- router' ∷
-- --   ∀ doc handlers handlers' m renderers request resCodecs.
-- --   Monad m ⇒
-- --   HMap (ArgMapping (Variant request → String)) { | handlers } { | handlers' } ⇒
-- --   HFoldlWithIndex (RouterFolding (RouterInterface request) handlers' resCodecs renderers) Unit (Variant request) (m (Api.Server.Result (Lazy String /\ doc))) ⇒
-- --   Spec.Raw request resCodecs renderers →
-- --   { | handlers } →
-- --   Request.Duplex.Request →
-- --   m (Either RoutingError (Api.Server.Result (Lazy String /\ doc)))
-- -- router' spec@(Spec.Raw { codecs }) handlers =
-- --   let
-- --     print ∷ Variant request → String
-- --     print route = Client.Router.print codecs.request (Hybrid.HTTP.Exchange route Nothing)
-- -- 
-- --     handlers' ∷ { | handlers' }
-- --     handlers' = hmap (ArgMapping print) handlers
-- -- 
-- --     fakeClientRouter ∷ RouterInterface request
-- --     fakeClientRouter =
-- --       { navigate: const $ pure unit
-- --       , redirect: const $ pure unit
-- --       , print
-- --       , submit: const $ pure unit
-- --       }
-- --   in
-- --     router fakeClientRouter spec handlers'
