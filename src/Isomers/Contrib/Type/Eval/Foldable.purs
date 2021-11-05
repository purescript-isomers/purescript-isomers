module Isomers.Contrib.Type.Eval.Foldable where

import Isomers.Contrib.Heterogeneous.List (HCons, HNil)
import Isomers.Contrib.Type.Data.Maybe (Just', MProxy, Nothing')
import Prim.RowList (Cons, Nil) as RL
import Type.Eval (class Eval, TypeExpr)
import Type.Eval.Boolean (Bool, TrueExpr, FalseExpr)
import Type.Eval.Foldable (Foldr, FoldrWithIndex)
import Type.Prelude (Proxy)

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
-- |
-- | TODO: Drop `'` prime from the name because it is
-- | misleading - we don't have a strict foldr here
-- | but we have strict Foldl below which is marked
-- | by "usual" prime.
foreign import data Foldr' ∷ (Type → TypeExpr → TypeExpr) → TypeExpr → Type → TypeExpr


--instance foldr_Tuple ::
--  ( Eval (fn a (Foldr fn z b)) ty
--  ) =>
--  Eval (Foldr fn z (Tuple a b)) ty

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
else instance foldrNothing ∷ Eval acc ty ⇒
  Eval (Foldr' fn acc (MProxy Nothing')) ty
else instance foldrJust ∷ (Eval (fn a (Foldr' fn acc (MProxy Nothing'))) ty) ⇒
  Eval (Foldr' fn acc (MProxy (Just' a))) ty
else instance foldRFoldrFallack ∷
  (Eval (Foldr f acc a) ty) ⇒
  Eval (Foldr' f acc a) ty


-- | A strict version of foldl...
-- | Just a quick signature reminder ;-)
-- | foldl' ∷ (acc → b → acc) → acc → f b → acc
foreign import data Foldl' ∷ (Type → Type → TypeExpr) → Type → Type → TypeExpr

instance foldlRowListCons' ::
  ( Eval (fn acc a) acc'
  , Eval (Foldl' fn acc' (Proxy rl)) ty
  ) =>
  Eval (Foldl' fn acc (Proxy (RL.Cons sym a rl))) ty

instance foldlRowListNil :: Eval (Foldl' fn acc (Proxy RL.Nil)) acc
