module Isomers.Contrib.Type.Eval.Foldable where

import Isomers.Contrib.Heterogeneous.List (HCons, HNil)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Boolean (Bool, TrueExpr, FalseExpr)
import Type.Eval.Foldable (Foldr, FoldrWithIndex)

foreign import data SomeFoldWithIndex ∷ (Type → Type → TypeExpr) → Type → Type → TypeExpr → TypeExpr

instance someWithIndexFold ::
  ( Eval (fn idx a) a'
  , Eval (Bool TrueExpr b a') c
  ) =>
  Eval (SomeFoldWithIndex fn idx a b) c

type SomeWithIndex (f ∷ Type → Type → TypeExpr) =
  FoldrWithIndex (SomeFoldWithIndex f) FalseExpr


-- | Local Foldr' which fallsback to the original one
-- | but allows us to add more instances.
foreign import data Foldr' ∷ (Type → TypeExpr → TypeExpr) → TypeExpr → Type → TypeExpr

instance foldrHConsMany ::
  ( Eval (fn a (Foldr' fn z (HCons b t))) ty
  ) =>
  Eval (Foldr' fn z (HCons a (HCons b t))) ty
else instance foldrHConsOne ::
  ( Eval (fn a (Foldr' fn z HNil)) ty
  ) =>
  Eval (Foldr' fn z (HCons a HNil)) ty
else instance foldrHNil ::
  ( Eval z ty
  ) =>
  Eval (Foldr' fn z HNil) ty
else instance foldRFoldrFallack ∷
  (Eval (Foldr f acc a) ty) ⇒
  Eval (Foldr' f acc a) ty
