module Isomer.Contrib.Type.Eval.Foldable where

import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Boolean (Bool, TrueExpr, FalseExpr)
import Type.Eval.Foldable (FoldrWithIndex)

foreign import data SomeFoldWithIndex ∷ (Type → Type → TypeExpr) → Type → Type → TypeExpr → TypeExpr

instance someWithIndexFold ::
  ( Eval (fn idx a) a'
  , Eval (Bool TrueExpr b a') c
  ) =>
  Eval (SomeFoldWithIndex fn idx a b) c

type SomeWithIndex (f ∷ Type → Type → TypeExpr) =
  FoldrWithIndex (SomeFoldWithIndex f) FalseExpr
