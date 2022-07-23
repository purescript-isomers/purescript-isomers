module Isomers.Server.Handler where

import Prelude

import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Type.Equality (to) as Type.Equality
import Type.Eval (class Eval, Lift, kind TypeExpr)
import Type.Eval.Foldable (Foldr)
import Type.Eval.Function (type (<<<)) as E
import Type.Eval.RowList (FromRow)
import Type.Prelude (class TypeEquals, Proxy)

type Handler m req res = req -> m res

foreign import data UnifyMonadStep :: forall k. Type -> TypeExpr k -> TypeExpr k

type UnifyMonad m row = (Foldr UnifyMonadStep (Lift (m Unit)) E.<<< FromRow) row

instance evalUnifyMonadRec ::
  ( Eval acc (m Unit)
  , Eval (UnifyMonad m rec) (m Unit)
  ) =>
  Eval (UnifyMonadStep { | rec } acc) (m Unit)
else instance evalMonad ::
  ( TypeEquals f (a -> m b)
  , Eval acc (m Unit)
  ) =>
  Eval (UnifyMonadStep f acc) (m Unit)

-- | A helper which makes inferred type for handlers record nicer.
unifyMonad :: forall m r. Eval (UnifyMonad m r) (m Unit) => { | r } -> { | r }
unifyMonad r = r

-- | A mapping which allows partial and local interpretation of the handlers record.
-- | We do monad unification here as well so you don't have to apply `UnifyMonad`
-- | mapping as a separate step.
data InterpretHandler m m' = InterpretHandler (m ~> m')

instance mappingInterpretHandler ::
  TypeEquals (n a) (m a) =>
  Mapping (InterpretHandler m m') (req -> n a) (req -> m' a) where
  mapping (InterpretHandler interpreter) f = map (interpreter <<< Type.Equality.to) f
else instance mappingInterpretHandlerRec ::
  HMap (InterpretHandler m m') { | rec } { | rec' } =>
  Mapping (InterpretHandler m m') { | rec } { | rec' } where
  mapping i rec = hmap i rec
