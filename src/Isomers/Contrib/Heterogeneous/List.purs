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

instance hfoldlHNil :: HFoldl f acc HNil acc where
  hfoldl _ acc _ = acc
else instance hfoldlHConsOne ::
  Folding f acc a z =>
  HFoldl f acc (HCons a HNil) z where
  hfoldl f acc (HCons a _) = folding f acc a
else instance hfoldlHConsMany ::
  ( Folding f acc a acc'
  , HFoldl f acc' (HCons b c) acc''
  ) =>
  HFoldl f acc (HCons a (HCons b c)) acc'' where
  hfoldl f acc (HCons a rest) = hfoldl f (folding f acc a) rest

instance hmapHNil :: HMap f HNil HNil where
  hmap _ x = x
else instance hmapHCons ::
  ( Mapping f a b
  , HMap f t t'
  ) =>
  HMap f (HCons a t) (b : t') where
  hmap f (a : t) = mapping f a : hmap f t

