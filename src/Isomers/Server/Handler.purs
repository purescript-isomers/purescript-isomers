module Isomers.Server.Handler where

import Prelude

import Type.Eval (class Eval, Lift, kind TypeExpr)
import Type.Eval.Foldable (Foldr)
import Type.Eval.Function (type (<<<)) as E
import Type.Eval.RowList (FromRow)
import Type.Prelude (class TypeEquals, RProxy)

type Handler m req res = req → m res

foreign import data UnifyMonadStep ∷ Type → TypeExpr → TypeExpr

type UnifyMonad m row
  = (Foldr UnifyMonadStep (Lift (m Unit)) E.<<< FromRow) (RProxy row)

instance evalUnifyMonadRec ∷
  ( Eval acc (m Unit)
  , Eval (UnifyMonad m rec) (m Unit)
  ) ⇒
  Eval (UnifyMonadStep { | rec } acc) (m Unit)
else instance evalMonad ∷
  ( TypeEquals f (a → m b)
  , Eval acc (m Unit)
  ) ⇒
  Eval (UnifyMonadStep f acc) (m Unit)

-- | A helper which makes inferred type for handlers record nicer.
unifyMonad ∷ ∀ m r. Eval (UnifyMonad m r) (m Unit) ⇒ { | r } → { | r }
unifyMonad r = r
