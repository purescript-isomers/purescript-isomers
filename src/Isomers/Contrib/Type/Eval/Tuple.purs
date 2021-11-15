module Isomers.Contrib.Type.Eval.Tuple where

import Data.Tuple (Tuple)
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map) as Eval.Functor
import Type.Eval.RowList (FromRow, ToRow) as Eval.RowList
import Type.Eval.Tuple (Curry, Tuple')

foreign import data Tuple'' :: forall a b. a -> b -> TypeExpr (Tuple a b)

type Tuples a
  = Eval.RowList.ToRow <<< Eval.Functor.Map (Tuple'' a) <<< Eval.RowList.FromRow



