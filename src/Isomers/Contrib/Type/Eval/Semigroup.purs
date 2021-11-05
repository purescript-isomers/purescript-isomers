module Isomers.Contrib.Type.Eval.Semigroup where

import Prim.RowList as RL
import Prim.Symbol (class Append) as S
import Type.Eval (class Eval, TypeExpr)
import Type.Prelude (Proxy)

foreign import data Append :: Type -> Type -> TypeExpr

infixr 5 type Append as <>

instance append_RowList_Nil :: Eval (Append (Proxy RL.Nil) (Proxy (tail))) (Proxy tail)
else instance append_RowList_Cons ::
  ( Eval (Append (Proxy t) (Proxy tail)) (Proxy tail')
    ) =>
  Eval (Append (Proxy (RL.Cons s a t)) (Proxy tail)) (Proxy (RL.Cons s a tail'))

instance append_Symbol ::
  ( S.Append s1 s2 s3
    ) =>
  Eval (Append (Proxy s1) (Proxy s2)) (Proxy s3)
