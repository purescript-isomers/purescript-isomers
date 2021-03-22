module Isomers.Spec.Record where

import Prelude

import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Contrib.Type.Eval.Foldable (Foldl')
import Isomers.Request.Duplex.Generic (PrefixRoutes) as Request.Duplex.Generic
import Isomers.Request.Duplex.Variant (empty, on) as Request.Duplex.Variant
import Isomers.Spec.Spec (Spec(..))
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Record (insert) as Record
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy)
import Type.Row (RProxy)

foreign import data SubspecInputStep ∷ Type → Type → TypeExpr

type SubspecInput row
  = (Foldl' SubspecInputStep Unit <<< FromRow) (RProxy row)

instance evalSubspecUnit ∷ Eval (SubspecInputStep Unit (Spec i req res)) i
else instance evalSubspecI ∷ (TypeEquals i j) ⇒ Eval (SubspecInputStep i (Spec j req res)) j

data SpecFolding
  = SpecFolding Request.Duplex.Generic.PrefixRoutes

-- | We assume here that record fields are already `Spec` values.

instance specFoldingSpec ∷
  ( Row.Cons l req () lreq
  , Row.Cons l req vReq vReq'
  , Row.Union vReq lreq vReq'
  , Row.Cons l res rRes rRes'
  , Row.Lacks l rRes
  , IsSymbol l
  ) ⇒
  FoldingWithIndex
    SpecFolding
    (SProxy l)
    (Spec r (Variant vReq) { | rRes })
    (Spec r req res)
    (Spec r (Variant vReq') { | rRes' }) where
  foldingWithIndex sf@(SpecFolding pr) l (Spec acc) (Spec { request, response }) = Spec
      { response: Record.insert l response acc.response
      , request:
          acc.request
          # Request.Duplex.Variant.on l request
      }

spec ∷
  ∀ rec i req res.
  Eval (SubspecInput rec) i ⇒
  HFoldlWithIndex SpecFolding (Spec i (Variant ()) {}) { | rec } (Spec i req res) ⇒
  Boolean →
  { | rec } →
  Spec i req res
spec b = hfoldlWithIndex (SpecFolding b) (Spec { request: Request.Duplex.Variant.empty, response: {} } ∷ Spec i (Variant ()) {})


