module Isomers.Contrib.Type.Eval.Foldings where

import Prelude

import Data.Variant.Prefix (NilExpr)
import Isomers.Contrib.Type.Eval.Foldable (Foldr')
import Prim.RowList (Cons) as RL
import Record.Extra (type (:::), SLProxy, SNil)
import Type.Eval (class Eval, Lift, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (RLProxy, SProxy)

-- | Iterate over symbols a build a row with a give type.
foreign import data HomogeneousRowStep ∷ Type → Type → TypeExpr → TypeExpr

instance evalHomogeneousRowStep ∷
  (Eval te (RLProxy t)) ⇒
  Eval (HomogeneousRowStep a (SProxy l) te) (RLProxy (RL.Cons l a t))

type HomogeneousRow a = ToRow <<< Foldr' (HomogeneousRowStep a) NilExpr


-- | Create sorted SList.
foreign import data SListCons ∷ Type → TypeExpr → TypeExpr
instance evalToSList ∷ (Eval t (SLProxy t')) ⇒ Eval (SListCons (SProxy s) t)  (SLProxy (s ::: t'))

type ToSortedSList hlist = (Foldr' SListCons (Lift (SLProxy SNil)) <<< FromRow <<< HomogeneousRow Unit) hlist

