module Isomers.Contrib.Heterogeneous.List where

import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap, mapping)

data HCons a b
  = HCons a b

data HNil
  = HNil

infixr 8 HCons as :

infixr 8 type HCons as :

-- | Rip from `purescript-heterogeneous/test/HList.purs`

instance hfoldlHNil :: HFoldl f acc HNil acc where
  hfoldl _ acc _ = acc
else instance hfoldlHConsOne ::
  Folding f acc a z =>
  HFoldl f acc (a : HNil) z where
  hfoldl f acc (a : _) = folding f acc a
else instance hfoldlHConsMany ::
  ( Folding f acc a acc'
  , HFoldl f acc' (b : c) acc''
  ) =>
  HFoldl f acc (a : b : c) acc'' where
  hfoldl f acc (a : rest) = hfoldl f (folding f acc a) rest

instance hmapHNil :: HMap f HNil HNil where
  hmap _ x = x
else instance hmapHConsOne ::
  ( Mapping f a a'
  ) =>
  HMap f (a : HNil) (a' : HNil) where
  hmap f (a : HNil) = mapping f a : HNil
else instance hmapHCons ::
  ( Mapping f a a'
  , HMap f (b : t) (b' : t')
  ) =>
  HMap f (a : b : t) (a' : b' : t') where
  hmap f (a : t) = mapping f a : hmap f t

