module Isomers.Contrib.Heterogeneous.List where

import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap, mapping)

-- | Rip from `purescript-heterogeneous/test/HList.purs`
data HCons a b
  = HCons a b

data HNil
  = HNil

infixr 8 type HCons as :

infixr 8 HCons as :

instance hfoldlHNil :: HFoldl f x HNil x where
  hfoldl _ x _ = x
else instance hfoldlHConsOne ::
  Folding f x a z =>
  HFoldl f x (HCons a HNil) z where
  hfoldl f x (HCons a _) = folding f x a
else instance hfoldlHConsMany ::
  ( Folding f x a y
  , HFoldl f y (HCons b c) z
  ) =>
  HFoldl f x (HCons a (HCons b c)) z where
  hfoldl f x (HCons a rest) = hfoldl f (folding f x a) rest

instance hmapHNil :: HMap f HNil HNil where
  hmap _ x = x
else instance hmapHCons ::
  ( Mapping f a b
  , HMap f t t'
  ) =>
  HMap f (HCons a t) (b : t') where
  hmap f (a : t) = mapping f a : hmap f t
