module Isomers.Contrib.Heterogeneous.Mappings.Newtype where

import Data.Newtype (class Newtype, unwrap, wrap)
import Heterogeneous.Mapping (class Mapping)

data Unwrap = Unwrap

instance unwrapMapping ∷ (Newtype n a) ⇒ Mapping Unwrap n a where
  mapping _ v = unwrap v


data Wrap = Wrap

instance wrapMapping ∷ (Newtype n a) ⇒ Mapping Wrap a n where
  mapping _ v = wrap v
