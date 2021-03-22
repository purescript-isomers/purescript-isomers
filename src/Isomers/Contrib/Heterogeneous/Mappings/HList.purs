module Isomers.Contrib.Heterogeneous.Mappings.HList where

import Heterogeneous.Mapping (class Mapping)
import Isomers.Contrib.Heterogeneous.List (HNil(..), type (:), (:))

newtype ConsMapping a = ConsMapping a

instance mappingHNil ∷ Mapping (ConsMapping a) HNil (a : HNil) where
  mapping (ConsMapping a) _ = a : HNil
instance mappingCons ∷ Mapping (ConsMapping a) (b : t) (a : b : t) where
  mapping (ConsMapping a) t = a : t
