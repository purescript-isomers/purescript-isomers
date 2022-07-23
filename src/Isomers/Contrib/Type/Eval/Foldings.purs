module Isomers.Contrib.Type.Eval.Foldings where

import Prelude

import Data.Variant.Prefix (NilExpr)
import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Isomers.Contrib.Heterogeneous.List (HCons(..), HNil(..))
import Prim.RowList (Cons) as RL
import Prim.RowList (RowList)
import Type.Eval (class Eval, Lift, TypeExpr)
import Type.Eval.Dispatch (class Dispatch1, KindOf)
import Type.Eval.Foldable (Foldr)
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class TypeEquals, Proxy(..))

-- | Iterate over symbols a build a row with a give type.
foreign import data HomogeneousRowListStep :: forall a. a -> Symbol -> TypeExpr (RowList a) -> TypeExpr (RowList a)

instance evalHomogeneousRowListStep ::
  ( Eval te t
  ) =>
  Eval (HomogeneousRowListStep a l te) (RL.Cons l a t)

type HomogeneousRowList a = Foldr (HomogeneousRowListStep a) NilExpr

type HomogeneousRow a = ToRow <<< HomogeneousRowList a

-- | Move these to `Contrib.Type.Eval.HList`
-- | We need a lifted version of `Heterogeneous.HList` which is always just
-- | a `Type` or `Type → Type → Type`. But we want to lift it to kind
-- | `HList k` so we can process it using `typelevel-eval`.
data HList a

foreign import data HCons' :: forall t. t -> HList t -> HList t
foreign import data HNil' :: forall k. HList k

foreign import data FromHListType :: Type -> TypeExpr (HList Type)

instance Eval (FromHListType t) t' => Eval (FromHListType (HCons h t)) (HCons' h t')
else instance Eval (FromHListType HNil) HNil'

class LiftHList :: forall k. Type -> HList k -> Constraint
class LiftHList hlist hlist' | hlist -> hlist'

instance (LiftHList t t') => LiftHList (HCons h t) (HCons' h t')
instance LiftHList HNil HNil'

instance
  ( Eval (f a (Foldr f acc tail)) acc'
  ) =>
  Dispatch1 HList (Foldr f acc ((HCons' a tail))) acc'
else instance
  ( Eval acc acc''
  ) =>
  Dispatch1 HList (Foldr f acc HNil') acc''

instance
  ( Eval (f a) a'
  , Eval (Map f tail) tail'
  ) =>
  Dispatch1 HList (Map f ((HCons' a tail))) (HCons' a' tail')
else instance Dispatch1 HList (Map f HNil') HNil'

-- | We need this proxy so we can dispatch heterogenous 
-- | `Folding` over `HList`.
data HListProxy :: forall k. HList k -> Type
data HListProxy a = HListProxy

instance
  ( Folding f x (Proxy head) z
  , HFoldl f z (HListProxy tail) res
  ) =>
  HFoldl f x (HListProxy (HCons' head tail)) res
  where
  hfoldl f acc _ =
    hfoldl f
      (folding f acc (Proxy :: Proxy head))
      (HListProxy :: HListProxy tail)
else instance
  HFoldl f acc (HListProxy HNil') acc
  where
  hfoldl _ acc _ = acc

