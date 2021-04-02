module Isomers.Contrib.Heterogeneous.Mappings where

import Prelude

import Data.Functor.App (App(..))
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap, mapping)

data Compose f g
  = Compose f g

instance mappingCompose ∷ (Mapping g a b, Mapping f b c) ⇒ Mapping (Compose f g) a c where
  mapping (Compose f g) = mapping f <<< mapping g


newtype ConstValue a = ConstValue a

instance mappingConstValue ∷ Mapping (ConstValue a) x a where
  mapping (ConstValue a) _ = a

