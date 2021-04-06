module Isomers.Spec.Builder where

import Prelude

import Data.Bifunctor (lmap)
import Data.Lens (over) as Lens
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Heterogeneous.Folding (class HFoldl)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, hmap)
import Isomers.Contrib.Heterogeneous.List (type (:))
import Isomers.HTTP (Method(..))
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.Request (Accum, Duplex(..), Parser, Printer, Duplex') as Request
import Isomers.Request.Accum (root, unifyRoute) as Request.Accum
import Isomers.Request.Accum.Generic (class HFoldlVariantStep, MethodStep)
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Accept (RequestParserFolding, RequestPrinterFolding, RequestRouteStep, ResponseContentTypesMapping)
import Isomers.Spec.Accept (ResponseRecordStep) as Accept
import Isomers.Spec.Accept (spec) as Spec.Accept
import Isomers.Spec.Method (spec) as Spec.Method
import Isomers.Spec.Record (SubspecBody, UnifyRouteType)
import Isomers.Spec.Record (spec) as Spec.Record
import Isomers.Spec.Type (ResponseMapping, Spec(..), RequestMapping)
import Isomers.Spec.Type (insert, unifyRoute) as Type
import Prim.Row (class Cons, class Lacks) as Row
import Type.Equality (class TypeEquals)
import Type.Equality (to) as Type.Equality
import Type.Eval (class Eval)
import Type.Prelude (class IsSymbol, Proxy(..), RProxy, SProxy(..))

-- | Used for recursive mapping over a record fields
data SpecStep route
  = SpecStep

instance mappingSpecMapping ∷ Builder a body route ireq oreq res ⇒ Mapping (SpecStep route) a (Spec body route ireq oreq res) where
  mapping = spec

class Builder a body route ireq oreq res | a → body ireq oreq res where
  spec ∷ SpecStep route → a → Spec body route ireq oreq res

-- | TODO: Can we hide these constraint details?
-- | For more info regarding this processing please check
-- | docs for `Spec.Accept.response` and `Spec.Accept.request`.
instance builderHListToAcceptSpec ∷
  ( HMap ResponseContentTypesMapping (h : t) cts
  , HFoldl (RequestRouteStep ireq route) (Variant () → route) cts (Variant ivReq → route)
  , HFoldl
      (RequestParserFolding body route oreq)
      (MediaPattern → Request.Parser body (route → Variant ()))
      cts
      (MediaPattern → Request.Parser body (route → Variant ovReq))
  , HFoldl Accept.ResponseRecordStep {} (h : t) res
  , HFoldl
      (RequestPrinterFolding ireq)
      (Variant () → Request.Printer)
      cts
      (Variant ivReq → Request.Printer)
  , TypeEquals route_ route
  ) ⇒
  Builder (Request.Accum body route_ ireq oreq /\ (h : t)) body route (Variant ivReq) (Variant ovReq) res where
  spec _ = Spec.Accept.spec <<< lmap Request.Accum.unifyRoute
-- | A spec which doesn't care about accept header.
else instance builderPlainEndpoint ∷
  Builder (Request.Accum body route ireq oreq /\ Response.Duplex ct ires ores) body route ireq oreq (Response.Duplex ct ires ores) where
  spec _ (request /\ response) = Spec { request, response }
else instance builderMethodRec ∷
  ( HMap (SpecStep route) { | rec } { | specs }
  -- , Eval (UnifyRouteType specs) route
  , Eval (SubspecBody specs) (RProxy body)
  , HMap ResponseMapping { | specs } { | resDpls }
  , HMap RequestMapping { | specs } reqDpls
  , HMapWithIndex MethodStep reqDpls reqDpls'
  , HFoldlVariantStep body route reqDpls' vi vo
  ) ⇒
  Builder (Method { | rec }) body route (Variant vi) (Variant vo) { | resDpls } where
  spec s (Method rec) = do
    let
      specs ∷ { | specs }
      specs = hmap s rec
    Spec.Method.spec (Method specs)

-- | Noop - just return given spec. Useful when folding nested specs.
instance builderPlainSpec ∷
  Builder (Spec body route ireq oreq res) body route ireq oreq res where
  spec _ s = s

-- | We map over a record so we get record of Specs.
-- | Next we fold the record to get the final Spec.
instance recBuilderRecord ∷
  ( HMap (SpecStep route) { | rec } { | rec' }
  , Eval (UnifyRouteType rec') route
  , Eval (SubspecBody rec') (RProxy body)
  , HMap ResponseMapping { | rec' } { | res }
  , HMap RequestMapping { | rec' } { | reqs }
  , HFoldlVariantStep body route { | reqs } ivreq ovreq
  ) ⇒
  Builder { | rec } body route (Variant ivreq) (Variant ovreq) { | res } where
  spec s r = Spec.Record.spec true r'
    where
    r' = hmap s r


data Insert (l ∷ Symbol) a sub = Insert (∀ body. Request.Duplex' body a) sub

instance insertSpecBuilderSpec ∷
  ( Row.Cons l a route route'
  , IsSymbol l
  , Row.Lacks l route
  ) ⇒
  Builder (Insert l a (Spec body { | route' } ireq oreq res)) body { | route } ireq oreq res where
  spec s (Insert dpl sub) = Type.insert (SProxy ∷ SProxy l) dpl sub
else instance insertSpecBuilder ∷
  ( Row.Cons l a route route'
  , Builder sub body { | route' } ireq oreq res
  , IsSymbol l
  , Row.Lacks l route
  ) ⇒
  Builder (Insert l a sub) body { | route } ireq oreq res where
  spec s (Insert dpl sub) = Type.insert (SProxy ∷ SProxy l) dpl (spec (SpecStep ∷ SpecStep { | route' }) sub)

root ∷ ∀ a rb ireq route oreq res. Builder a rb route ireq oreq res ⇒ a → Spec rb route ireq oreq res
root a = do
  let
    Spec { request, response } = spec (SpecStep ∷ SpecStep route) a

    request' = Request.Accum.root request
  Spec { request: request', response }

insertBuilder ∷ ∀ a l sub. SProxy l → (∀ body. Request.Duplex' body a) → sub → Insert l a sub
insertBuilder l dpl sub = Insert dpl sub
