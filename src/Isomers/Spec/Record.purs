module Isomers.Spec.Record where

import Prelude

import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, hmap)
import Isomers.Contrib.Type.Eval.Foldable (Foldl')
import Isomers.Request.Accum.Generic (class HFoldlVariantStep)
import Isomers.Request.Accum.Generic (variant) as Request.Accum.Generic
import Isomers.Spec.Type (RequestMapping, ResponseMapping, Spec(..), _RequestMapping, _ResponseMapping)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Prelude (class TypeEquals)
import Type.Row (RProxy)

foreign import data UnifyAccTypeStep ∷ Type → Type → TypeExpr

type UnifyRouteType row
  = (Foldl' UnifyAccTypeStep Unit <<< FromRow) (RProxy row)

instance evalSubspecUnit ∷ Eval (UnifyAccTypeStep Unit (Spec body route ireq oreq res)) route
else instance evalSubspecI ∷ (TypeEquals route_ route, TypeEquals route route_) ⇒ Eval (UnifyAccTypeStep route_ (Spec body route ireq oreq res)) route


-- foreign import data UnifyOutReqTypeStep ∷ Type → Type → TypeExpr
-- 
-- type UnifyOutReqType row
--   = (Foldl' UnifyOutReqTypeStep Unit <<< FromRow) (RProxy row)
-- 
-- instance evalSubspecOutReqInit ∷ Eval (UnifyOutReqTypeStep Unit (Spec body ireq acc oreq res)) ireq
-- else instance evalSubspecOutReqNext ∷ (TypeEquals ireq ireq_) ⇒ Eval (UnifyOutReqTypeStep ireq_ (Spec body ireq acc oreq res)) ireq
-- 

foreign import data SubspecBodyStep ∷ Type → Type → TypeExpr

type SubspecBody row
  = (Foldl' SubspecBodyStep Unit <<< FromRow) (RProxy row)

instance evalSubspecBodyUnit ∷ Eval (SubspecBodyStep Unit (Spec body route ireq oreq res)) (RProxy body)
else instance evalSubspecBodyStep ∷ (TypeEquals (RProxy body) (RProxy body')) ⇒ Eval (SubspecBodyStep (RProxy body) (Spec body' route ireq oreq res)) (RProxy body)

type PrefixRoutes = Boolean

-- | 1. We assume here that record fields are already `Spec` values.
-- |
-- | 2. We map over this record of specs to merge them by performing
-- | these steps:
-- |
-- | * Extract request duplexes from specs so we get a record of
-- | duplexes _RequestMapping.
-- |
-- | * Apply `Request.Duplex.Generic.variant` on the result so we
-- | end up with a single `Request.Duplex`.
-- |
-- | * Map over an original record to extract only response duplexes
-- | which is a value which we want to pass to the final spec record.
spec ∷
  ∀ rb rec reqs res route ivreq ovreq.
  -- Eval (UnifyRouteType rec) route ⇒
  Eval (SubspecBody rec) (RProxy rb) ⇒
  HMap ResponseMapping { | rec } { | res } ⇒
  HMap RequestMapping { | rec } { | reqs } ⇒
  HFoldlVariantStep rb route { | reqs } ivreq ovreq ⇒
  Boolean →
  { | rec } →
  Spec rb route (Variant ivreq) (Variant ovreq) { | res }
spec b r = do
  let
    reqs ∷ { | reqs }
    reqs = hmap _RequestMapping r
  Spec
    { response: hmap _ResponseMapping r
    , request: Request.Accum.Generic.variant b reqs
    }

