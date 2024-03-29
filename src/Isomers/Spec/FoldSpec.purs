module Isomers.Spec.FoldSpec where

import Prelude

import Data.Bifunctor (lmap)
import Data.Homogeneous (class ToHomogeneousRow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, hmap)
import Isomers.Contrib.Heterogeneous.List (type (:))
import Isomers.Contrib.Type.Eval.Foldings (FromHListType, HListProxy, HomogeneousRow)
import Isomers.HTTP (Method(..))
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.Request (Accum(..), Duplex, Duplex', Parser) as Request
import Isomers.Request.Accum (insert, insertReq, scalar, unifyRoute) as Request.Accum
import Isomers.Request.Accum.Generic (class HFoldlAccumVariant)
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Accept (RequestMediaPatternParser, ResponseContentTypeRecord) as Accept
import Isomers.Spec.Accept (ResponseContentTypeStep)
import Isomers.Spec.Accept (accumSpec) as Spec.Accept
import Isomers.Spec.Method (MethodStep)
import Isomers.Spec.Method (accumSpec) as Spec.Method
import Isomers.Spec.Record (accumSpec) as Spec.Record
import Isomers.Spec.Types (AccumSpec(..), Spec, rootAccumSpec)
import Isomers.Spec.Types.Mappings (GetRequest, GetResponse)
import Prim.Row (class Cons, class Lacks) as Row
import Type.Equality (class TypeEquals)
import Type.Equality (to) as Type.Equality
import Type.Eval (class Eval)
import Type.Eval.Function (type (<<<)) as T
import Type.Eval.Functor (Map)
import Type.Prelude (class IsSymbol, Proxy(..))

-- | Used for recursive mapping over a record fields.
-- | `route` variable is carried around because it helps inference.
data FoldSpecStep :: forall k. k -> Type
data FoldSpecStep route = FoldSpecStep

instance
  FoldSpec a route ireq oreq res =>
  Mapping (FoldSpecStep route) a (AccumSpec route ireq oreq res) where
  mapping = accumSpec

class FoldSpec a route ireq oreq res | a -> ireq oreq res where
  accumSpec :: FoldSpecStep route -> a -> AccumSpec route ireq oreq res

unifyRoute
  :: forall route route' t12 t13 t14
   . TypeEquals route route'
  => AccumSpec route t14 t13 t12
  -> AccumSpec route' t14 t13 t12
unifyRoute (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.unifyRoute request
    , response
    }

-- | TODO: We probably want to restrict this to segments only
data WithBody (l :: Symbol) i o = WithBody (Request.Duplex i o)

withBody :: forall i l o. Proxy l -> Request.Duplex i o -> WithBody l i o
withBody _ dpl = WithBody dpl

-- | Build an accept spec from request duplex and a hlist of response duplexes.
instance
  ( HFoldl Accept.ResponseContentTypeRecord {} (h : t) res
  , Eval ((Map ResponseContentTypeStep T.<<< FromHListType) (h : t)) cts
  , Eval (HomogeneousRow Void cts) sl
  , ToHomogeneousRow sl ireq ivReq
  , HFoldl
      (Accept.RequestMediaPatternParser route oreq)
      (MediaPattern -> Request.Parser (route -> Variant ()))
      (HListProxy cts)
      (MediaPattern -> Request.Parser (route -> Variant ovReq))
  , TypeEquals route_ route
  ) =>
  FoldSpec (Request.Accum route_ ireq oreq /\ (h : t)) route (Variant ivReq) (Variant ovReq) res where
  accumSpec _ = Spec.Accept.accumSpec <<< lmap Request.Accum.unifyRoute
else instance
  ( Row.Cons l i route ireq
  , Row.Cons l o route oreq
  , Row.Lacks l route
  , IsSymbol l
  , FoldSpec (Request.Accum { | route } { | ireq } { | oreq } /\ res) { | route } { | ireq } { | oreq } res'
  ) =>
  FoldSpec (WithBody l i o /\ res) { | route } { | ireq } { | oreq } res' where
  accumSpec s ((WithBody dpl) /\ res) = do
    let
      req = Request.Accum.insertReq (Proxy :: Proxy l) dpl
    accumSpec s (req /\ res :: Request.Accum { | route } { | ireq } { | oreq } /\ res)
-- | An endpoint which doesn't care about accept header.
else instance
  ( TypeEquals req (Request.Accum route ireq oreq)
  , TypeEquals res (Response.Duplex ct ires ores)
  ) =>
  FoldSpec (req /\ res) route ireq oreq (Response.Duplex ct ires ores) where
  accumSpec _ (request /\ response) =
    AccumSpec
      { request: Type.Equality.to request
      , response: Type.Equality.to response
      }

type Pass route = Request.Accum route route route

pass :: forall route. Pass route
pass = Request.Accum (pure identity) identity

instance
  ( FoldSpec (Pass route /\ Response.Duplex ct ires ores) route ireq oreq res
  ) =>
  FoldSpec (Response.Duplex ct ires ores) route ireq oreq res where
  accumSpec s response = accumSpec s ((pass /\ response) :: (Pass route /\ Response.Duplex ct ires ores))

instance
  ( FoldSpec (Pass route /\ (h : t)) route ireq oreq res
  ) =>
  FoldSpec ((h : t)) route ireq oreq res where
  accumSpec s response = accumSpec s ((pass /\ response) :: (Pass route /\ (h : t)))

instance
  ( HMap (FoldSpecStep route) { | rec } { | specs }
  , HMap GetResponse { | specs } { | resDpls }
  , HMap GetRequest { | specs } reqDpls
  , HMapWithIndex MethodStep reqDpls reqDpls'
  , HFoldlAccumVariant route reqDpls' vi vo
  ) =>
  FoldSpec (Method { | rec }) route (Variant vi) (Variant vo) { | resDpls } where
  accumSpec s (Method rec) = do
    let
      specs :: { | specs }
      specs = hmap s rec
    Spec.Method.accumSpec (Method specs)

-- | Noop - just return given spec. Useful when folding nested specs.
instance FoldSpec (AccumSpec route ireq oreq res) route ireq oreq res where
  accumSpec _ s = s

-- | We map over a record so we get record of Specs.
-- | Next we fold the record to get the final Spec.
instance
  ( HMap (FoldSpecStep route) { | rec } { | rec' }
  , HMap GetResponse { | rec' } { | res }
  , HMap GetRequest { | rec' } { | reqs }
  , HFoldlAccumVariant route { | reqs } ivreq ovreq
  ) =>
  FoldSpec { | rec } route (Variant ivreq) (Variant ovreq) { | res } where
  accumSpec s r = Spec.Record.accumSpec true r'
    where
    r' = hmap s r

-- | TODO: We probably want to restrict this to segments only
data Insert (l :: Symbol) a sub = Insert (Request.Duplex' a) sub

instance
  ( Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  ) =>
  FoldSpec (Insert l a (AccumSpec { | route' } ireq oreq res)) { | route } ireq oreq res where
  accumSpec _ (Insert dpl sub) = insertIntoAccumSpec (Proxy :: Proxy l) dpl sub
else instance
  ( Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  , FoldSpec sub { | route' } ireq oreq res
  ) =>
  FoldSpec (Insert l a sub) { | route } ireq oreq res where
  accumSpec _ (Insert dpl sub) =
    insertIntoAccumSpec
      (Proxy :: Proxy l)
      dpl
      (accumSpec (FoldSpecStep :: FoldSpecStep { | route' }) sub)

insert :: forall a l sub. Proxy l -> (Request.Duplex' a) -> sub -> Insert l a sub
insert _ dpl sub = Insert dpl sub

insertIntoAccumSpec
  :: forall a l ireq oreq route route' res
   . IsSymbol l
  => Row.Lacks l route
  => Row.Cons l a route route'
  => Proxy l
  -> Request.Duplex' a
  -> AccumSpec { | route' } ireq oreq res
  -> AccumSpec { | route } ireq oreq res
insertIntoAccumSpec l dpl (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.insert l dpl request
    , response
    }

data Scalar a sub = Scalar (Request.Duplex' a) sub

-- | We are not able to "match" on `{}` directly in the place of `route`.
-- | I hope that the use of `TypeEquals` is correct in this context.
instance
  ( TypeEquals {} route
  ) =>
  FoldSpec (Scalar a (AccumSpec a ireq oreq res)) route ireq oreq res where
  accumSpec _ (Scalar dpl sub) = unifyRoute (setScalarSpec dpl sub)
else instance
  ( FoldSpec sub a ireq oreq res
  , TypeEquals {} route
  ) =>
  FoldSpec (Scalar a sub) route ireq oreq res where
  accumSpec _ (Scalar dpl sub) =
    unifyRoute
      $ setScalarSpec
          dpl
          (accumSpec (FoldSpecStep :: FoldSpecStep a) sub)

setScalarSpec
  :: forall t13 t14 t15 t21
   . Request.Duplex' t21
  -> AccumSpec t21 t15 t14 t13
  -> AccumSpec (Record ()) t15 t14 t13
setScalarSpec dpl (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.scalar dpl request
    , response
    }

foldSpec
  :: forall t102 t103 t104 t106. FoldSpec t106 (Record ()) t104 t103 t102 => t106 -> Spec t104 t103 t102
foldSpec = rootAccumSpec <<< accumSpec (FoldSpecStep :: FoldSpecStep {})
