module Hybrid.Contrib.Heterogeneous.Mappings where

import Prelude

import Heterogeneous.Mapping (class Mapping, mapping)

data Compose f g = Compose f g

instance mappingCompose ∷ (Mapping g a b, Mapping f b c) ⇒ Mapping (Compose f g) a c where
  mapping (Compose f g) = mapping f <<< mapping g
