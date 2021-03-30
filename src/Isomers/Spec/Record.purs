module Isomers.Spec.Record where

import Prelude

import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, hmap)
import Isomers.Contrib.Type.Eval.Foldable (Foldl')
import Isomers.Request.Duplex.Generic (variant) as Request.Duplex.Generic
import Isomers.Request.Duplex.Generic (class HFoldlVariantStep)
import Isomers.Spec.Type (RequestMapping, ResponseMapping, Spec(..), _RequestMapping, _ResponseMapping)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Prelude (class TypeEquals)
import Type.Row (RProxy)

foreign import data SubspecInputStep ∷ Type → Type → TypeExpr

type SubspecInput row
  = (Foldl' SubspecInputStep Unit <<< FromRow) (RProxy row)

instance evalSubspecUnit ∷ Eval (SubspecInputStep Unit (Spec body i req res)) i
else instance evalSubspecI ∷ (TypeEquals i j) ⇒ Eval (SubspecInputStep i (Spec body j req res)) j

foreign import data SubspecBodyStep ∷ Type → Type → TypeExpr

type SubspecBody row
  = (Foldl' SubspecBodyStep Unit <<< FromRow) (RProxy row)

instance evalSubspecBodyUnit ∷ Eval (SubspecBodyStep Unit (Spec body i req res)) (RProxy body)
else instance evalSubspecBodyStep ∷ (TypeEquals (RProxy body) (RProxy body')) ⇒ Eval (SubspecBodyStep (RProxy body) (Spec body' j req res)) (RProxy body)

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
  ∀ rb rec i reqs res vreq.
  Eval (SubspecInput rec) i ⇒
  Eval (SubspecBody rec) (RProxy rb) ⇒
  HMap ResponseMapping { | rec } { | res } ⇒
  HMap RequestMapping { | rec } { | reqs } ⇒
  HFoldlVariantStep rb i { | reqs } vreq vreq ⇒
  Boolean →
  { | rec } →
  Spec rb i (Variant vreq) { | res }
spec b r = do
  let
    reqs ∷ { | reqs }
    reqs = hmap _RequestMapping r
  Spec
    { response: hmap _ResponseMapping r
    , request: Request.Duplex.Generic.variant b reqs
    }


