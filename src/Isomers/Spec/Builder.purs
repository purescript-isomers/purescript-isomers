module Isomers.Spec.Builder where

import Prelude
import Data.Bifunctor (lmap)
import Data.Homogeneous (class ToHomogeneousRow)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, hmap)
import Isomers.Contrib.Heterogeneous.List (type (:))
import Isomers.Contrib.Type.Eval.Foldings (HomogeneousRow)
import Isomers.HTTP (Method(..))
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.Request (Accum(..), Duplex, Duplex', Parser) as Request
import Isomers.Request.Accum (insert, insertReq, scalar, unifyRoute) as Request.Accum
import Isomers.Request.Accum.Generic (class HFoldlAccumVariant)
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Accept (RequestMediaPatternParser, ResponseContentType, ResponseContentTypeRecord) as Accept
import Isomers.Spec.Accept (accumSpec) as Spec.Accept
import Isomers.Spec.Method (MethodStep)
import Isomers.Spec.Method (accumSpec) as Spec.Method
import Isomers.Spec.Record (UnifyBody)
import Isomers.Spec.Record (accumSpec) as Spec.Record
import Isomers.Spec.Types (AccumSpec(..), GetRequest, GetResponse, Spec, rootAccumSpec)
import Prim.Row (class Cons, class Lacks) as Row
import Type.Equality (class TypeEquals)
import Type.Equality (to) as Type.Equality
import Type.Eval (class Eval)
import Type.Prelude (class IsSymbol, RProxy, SProxy(..))

-- | Used for recursive mapping over a record fields.
-- | `route` variable is carried around because it helps inference.
data BuilderStep route
  = BuilderStep

instance mappingBuilder ∷
  Builder a body route ireq oreq res ⇒
  Mapping (BuilderStep route) a (AccumSpec body route ireq oreq res) where
  mapping = accumSpec

class Builder a (body ∷ # Type) route ireq oreq res | a → body ireq oreq res where
  accumSpec ∷ BuilderStep route → a → AccumSpec body route ireq oreq res

unifyRoute ∷ ∀ route route' t12 t13 t14 t16. TypeEquals route route' ⇒ AccumSpec t16 route t14 t13 t12 → AccumSpec t16 route' t14 t13 t12
unifyRoute (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.unifyRoute request
    , response
    }

-- | TODO: We probably want to restrict this to segments only
data WithBody (l ∷ Symbol) (body ∷ # Type) i o
  = WithBody (Request.Duplex body i o)

withBody ∷ ∀ body i l o. SProxy l → Request.Duplex body i o → WithBody l body i o
withBody _ dpl = WithBody dpl

-- | Build an accept spec from request duplex and a hlist of response duplexes.
instance builderHListToAcceptSpec ∷
  ( HFoldl Accept.ResponseContentTypeRecord {} (h : t) res
  , HMap Accept.ResponseContentType (h : t) cts
  , Eval (HomogeneousRow Void cts) (RProxy sl)
  , ToHomogeneousRow sl ireq ivReq
  , HFoldl
      (Accept.RequestMediaPatternParser body route oreq)
      (MediaPattern → Request.Parser body (route → Variant ()))
      cts
      (MediaPattern → Request.Parser body (route → Variant ovReq))
  , TypeEquals route_ route
  ) ⇒
  Builder (Request.Accum body route_ ireq oreq /\ (h : t)) body route (Variant ivReq) (Variant ovReq) res where
  accumSpec _ = Spec.Accept.accumSpec <<< lmap Request.Accum.unifyRoute
else instance builderWithBodyEndpoint ∷
  ( Row.Cons l i route ireq
  , Row.Cons l o route oreq
  , Row.Lacks l route
  , IsSymbol l
  , Builder (Request.Accum body { | route } { | ireq } { | oreq } /\ res) body { | route } { | ireq } { | oreq } res'
  ) ⇒
  Builder (WithBody l body i o /\ res) body { | route } { | ireq } { | oreq } res' where
  accumSpec s ((WithBody dpl) /\ res) = do
    let
      req = Request.Accum.insertReq (SProxy ∷ SProxy l) dpl
    accumSpec s (req /\ res ∷ Request.Accum body { | route } { | ireq } { | oreq } /\ res)
-- | An endpoint which doesn't care about accept header.
else instance builderPlainEndpoint ∷
  ( TypeEquals req (Request.Accum body route ireq oreq)
  , TypeEquals res (Response.Duplex ct ires ores)
  ) ⇒
  Builder (req /\ res) body route ireq oreq (Response.Duplex ct ires ores) where
  accumSpec _ (request /\ response) =
    AccumSpec
      { request: Type.Equality.to request
      , response: Type.Equality.to response
      }

type Pass body route
  = Request.Accum body route route route

pass ∷ ∀ body route. Pass body route
pass = Request.Accum (pure identity) identity

instance builderPlainResponseEndpoint ∷
  (Builder (Pass body route /\ Response.Duplex ct ires ores) body route ireq oreq res) ⇒
  Builder (Response.Duplex ct ires ores) body route ireq oreq res where
  accumSpec s response = accumSpec s ((pass /\ response) ∷ (Pass body route /\ Response.Duplex ct ires ores))

instance builderResponseHListEndpoint ∷
  (Builder (Pass body route /\ (h : t)) body route ireq oreq res) ⇒
  Builder ((h : t)) body route ireq oreq res where
  accumSpec s response = accumSpec s ((pass /\ response) ∷ (Pass body route /\ (h : t)))

instance builderMethodRec ∷
  ( HMap (BuilderStep route) { | rec } { | specs }
  , Eval (UnifyBody specs) (RProxy body)
  , HMap GetResponse { | specs } { | resDpls }
  , HMap GetRequest { | specs } reqDpls
  , HMapWithIndex MethodStep reqDpls reqDpls'
  , HFoldlAccumVariant body route reqDpls' vi vo
  ) ⇒
  Builder (Method { | rec }) body route (Variant vi) (Variant vo) { | resDpls } where
  accumSpec s (Method rec) = do
    let
      specs ∷ { | specs }
      specs = hmap s rec
    Spec.Method.accumSpec (Method specs)

-- | Noop - just return given spec. Useful when folding nested specs.
instance builderPlainSpec ∷
  Builder (AccumSpec body route ireq oreq res) body route ireq oreq res where
  accumSpec _ s = s

-- | We map over a record so we get record of Specs.
-- | Next we fold the record to get the final Spec.
instance recBuilderRecord ∷
  ( HMap (BuilderStep route) { | rec } { | rec' }
  , Eval (UnifyBody rec') (RProxy body)
  , HMap GetResponse { | rec' } { | res }
  , HMap GetRequest { | rec' } { | reqs }
  , HFoldlAccumVariant body route { | reqs } ivreq ovreq
  ) ⇒
  Builder { | rec } body route (Variant ivreq) (Variant ovreq) { | res } where
  accumSpec s r = Spec.Record.accumSpec true r'
    where
    r' = hmap s r

-- | TODO: We probably want to restrict this to segments only
data Insert (l ∷ Symbol) a sub
  = Insert (∀ body. Request.Duplex' body a) sub

instance insertSpecBuilderSpec ∷
  ( Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  ) ⇒
  Builder (Insert l a (AccumSpec body { | route' } ireq oreq res)) body { | route } ireq oreq res where
  accumSpec s (Insert dpl sub) = insertIntoAccumSpec (SProxy ∷ SProxy l) dpl sub
else instance insertSpecBuilder ∷
  ( Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  , Builder sub body { | route' } ireq oreq res
  ) ⇒
  Builder (Insert l a sub) body { | route } ireq oreq res where
  accumSpec s (Insert dpl sub) =
    insertIntoAccumSpec
      (SProxy ∷ SProxy l)
      dpl
      (accumSpec (BuilderStep ∷ BuilderStep { | route' }) sub)

insert ∷ ∀ a l sub. SProxy l → (∀ body. Request.Duplex' body a) → sub → Insert l a sub
insert l dpl sub = Insert dpl sub

insertIntoAccumSpec ∷
  ∀ a body l ireq oreq route route' res.
  IsSymbol l ⇒
  Row.Lacks l route ⇒
  Row.Cons l a route route' ⇒
  SProxy l →
  Request.Duplex' body a →
  AccumSpec body { | route' } ireq oreq res →
  AccumSpec body { | route } ireq oreq res
insertIntoAccumSpec l dpl (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.insert l dpl request
    , response
    }

data Scalar a sub
  = Scalar (∀ body. Request.Duplex' body a) sub

-- | We are not able to "match" on `{}` directly in the place of `route`.
-- | I hope that the use of `TypeEquals` is correct in this context.
instance scalarBuilderSpec ∷
  (TypeEquals {} route) =>
  Builder (Scalar a (AccumSpec body a ireq oreq res)) body route ireq oreq res where
  accumSpec s (Scalar dpl sub) = unifyRoute (setScalarSpec dpl sub)
else instance scalarBuilder ∷
  ( Builder sub body a ireq oreq res
  , TypeEquals {} route
  ) =>
  Builder (Scalar a sub) body route ireq oreq res where
  accumSpec s (Scalar dpl sub) =
    unifyRoute
      $ setScalarSpec
          dpl
          (accumSpec (BuilderStep ∷ BuilderStep a) sub)

setScalarSpec ∷ ∀ t13 t14 t15 t17 t21. Request.Duplex' t17 t21 → AccumSpec t17 t21 t15 t14 t13 → AccumSpec t17 (Record ()) t15 t14 t13
setScalarSpec dpl (AccumSpec { request, response }) =
  AccumSpec
    { request: Request.Accum.scalar dpl request
    , response
    }

spec ∷ ∀ t102 t103 t104 t105 t106. Builder t106 t105 (Record ()) t104 t103 t102 ⇒ t106 → Spec t105 t104 t103 t102
spec = rootAccumSpec <<< accumSpec (BuilderStep ∷ BuilderStep {})
