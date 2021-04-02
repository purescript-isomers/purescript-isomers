module Isomers.Contrib.Heterogeneous.HMaybe where

import Heterogeneous.Folding (class Folding, class HFoldl, folding)
import Heterogeneous.Mapping (class HMap, class Mapping, mapping)
import Isomers.Contrib.Heterogeneous.Mappings (ConstValue(..))
import Type.Eval (Lift)
import Type.Proxy (Proxy)

data HJust a = HJust a
data HNothing = HNothing

type HJustExpr a = Lift (Proxy (HJust a))
type HNothingExpr = Lift (Proxy HNothing)

instance hfoldlHMaybe :: HFoldl f acc HNothing acc where
  hfoldl _ acc _ = acc
else instance hfoldlHJust ::
  Folding f acc a z =>
  HFoldl f acc (HJust a) z where
  hfoldl f acc (HJust a) = folding f acc a

instance hmapHNil :: HMap f HNothing HNothing where
  hmap _ x = x
else instance hmapHJust ::
  ( Mapping f a a'
  ) =>
  HMap f (HJust a) (HJust a') where
  hmap f (HJust a) = HJust (mapping f a)

class FromHMaybe b ma c | ma b → c where
  fromHMaybe ∷ b → ma → c

instance fromMaybeNothing ∷ FromHMaybe a HNothing a where
  fromHMaybe a _ = a

instance fromHMaybeJust ∷ FromHMaybe b (HJust a) a where
  fromHMaybe _ (HJust a) = a


