module Isomers.Contrib.Type.Eval.Foldings where

import Prelude

import Data.Variant.Prefix (NilExpr)
import Isomers.Contrib.Type.Eval.Foldable (Foldr')
import Prim.RowList (Cons) as RL
import Record.Extra (type (:::), SNil)
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (Proxy)

-- | Iterate over symbols a build a row with a give type.
foreign import data HomogeneousRowStep ∷ Type → Type → TypeExpr → TypeExpr

instance evalHomogeneousRowStep ∷
  (Eval te (Proxy t)) ⇒
  Eval (HomogeneousRowStep a (Proxy l) te) (Proxy (RL.Cons l a t))

type HomogeneousRow a = ToRow <<< Foldr' (HomogeneousRowStep a) NilExpr


-- | Create sorted SList.
foreign import data SListCons ∷ Type → TypeExpr → TypeExpr
instance evalToSList ∷ (Eval t (Proxy t')) ⇒ Eval (SListCons (Proxy s) t)  (Proxy (s ::: t'))

type ToSortedSList hlist = (Foldr' SListCons (Lift (Proxy SNil)) <<< FromRow <<< HomogeneousRow Unit) hlist

