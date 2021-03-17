module Isomer.Contrib.Type.Eval.Semigroup where

import Prim.RowList as RL
import Prim.Symbol (class Append) as S
import Type.Data.RowList (RLProxy)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Prelude (SProxy)

foreign import data Append :: Type -> Type -> TypeExpr

infixr 5 type Append as <>

instance append_RowList_Nil :: Eval (Append (RLProxy RL.Nil) (RLProxy (tail))) (RLProxy tail)
else instance append_RowList_Cons ::
  ( Eval (Append (RLProxy t) (RLProxy tail)) (RLProxy tail')
    ) =>
  Eval (Append (RLProxy (RL.Cons s a t)) (RLProxy tail)) (RLProxy (RL.Cons s a tail'))

instance append_Symbol ::
  ( S.Append s1 s2 s3
    ) =>
  Eval (Append (SProxy s1) (SProxy s2)) (SProxy s3)
