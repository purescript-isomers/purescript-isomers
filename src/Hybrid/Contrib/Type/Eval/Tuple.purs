module Hybrid.Contrib.Type.Eval.Tuple where

import Data.Tuple (Tuple)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (Id) as Eval.Function
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map) as Eval.Functor
import Type.Eval.RowList (FromRow, ToRow) as Eval.RowList

foreign import data Curry' :: (Type -> TypeExpr) -> Type -> Type -> TypeExpr

instance curry ::
  (Eval (f (Tuple a b)) c) ⇒
  Eval (Curry' f a b) c

type Tuples a
  = Eval.RowList.ToRow <<< Eval.Functor.Map (Curry' Eval.Function.Id a) <<< Eval.RowList.FromRow


foreign import data Uncurry' :: (Type → Type -> TypeExpr) -> Type -> TypeExpr

instance uncurry ::
  (Eval (f a b) c) ⇒
  Eval (Uncurry' f (Tuple a b)) c

