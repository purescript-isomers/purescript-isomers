module Isomers.Contrib.Type.Eval.Semigroup where

import Prim.RowList as RL
import Prim.Symbol (class Append) as S
import Type.Eval (class Eval, TypeExpr)

foreign import data Append :: ∀ a. a -> a -> TypeExpr a

infixr 5 type Append as <>

instance append_RowList_Nil :: Eval (Append RL.Nil tail) tail
else instance append_RowList_Cons ::
  ( Eval (Append t tail) tail'
    ) =>
  Eval (Append (RL.Cons s a t) tail) (RL.Cons s a tail')

instance append_Symbol ::
  ( S.Append s1 s2 s3
    ) =>
  Eval (Append s1 s2) s3
