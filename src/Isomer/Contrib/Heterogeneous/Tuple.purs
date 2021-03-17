module Isomer.Contrid.Heterogenous.Tuple where

import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class Folding)
import Heterogeneous.Mapping (class Mapping)
import Prelude (Unit)

newtype Cons a = Cons a

instance mappinCons ∷ Mapping (Cons a) b (a /\ b) where
  mapping (Cons a) b = (a /\ b)

newtype Snoc a = Snoc a

instance mappinSnoc ∷ Mapping (Snoc b) a (a /\ b) where
  mapping (Snoc b) a = (a /\ b)

data Fst = Fst

instance mappingFst ∷ Mapping Fst (a /\ b) a where
  mapping _ (a /\ b) = a

data Snd = Snd

instance mappingSnd ∷ Mapping Snd (a /\ b) b where
  mapping _ (a /\ b) = b

instance foldingFst ∷ Folding Fst Unit (a /\ b) a where
  folding _ _ (a /\ b) = a

instance foldingSnd ∷ Folding Snd Unit (a /\ b) b where
  folding _ _ (a /\ b) = b

