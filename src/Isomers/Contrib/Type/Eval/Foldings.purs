module Isomers.Contrib.Type.Eval.Foldings where

import Data.Variant.Prefix (NilExpr)
import Isomers.Contrib.Type.Eval.Foldable (Foldr')
import Prim.RowList (Cons) as RL
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (ToRow)
import Type.Prelude (RLProxy, SProxy)

foreign import data HomogeneousRowStep ∷ Type → Type → TypeExpr → TypeExpr

instance evalHomogeneousRowStep ∷
  (Eval te (RLProxy t)) ⇒
  Eval (HomogeneousRowStep a (SProxy l) te) (RLProxy (RL.Cons l a t))

type HomogeneousRow a lst
  = (ToRow <<< Foldr' (HomogeneousRowStep a) NilExpr) lst
