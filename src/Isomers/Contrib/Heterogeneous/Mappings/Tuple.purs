module Isomers.Contrib.Heterogeneous.Mappings.Tuple where

import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class Folding)
import Heterogeneous.Mapping (class Mapping)
import Prelude (Unit)

newtype Cons a = Cons a

instance mappinCons :: Mapping (Cons a) b (a /\ b) where
  mapping (Cons a) b = (a /\ b)

newtype Snoc a = Snoc a

instance mappinSnoc :: Mapping (Snoc b) a (a /\ b) where
  mapping (Snoc b) a = (a /\ b)

data Fst = Fst

instance mappingFst :: Mapping Fst (a /\ b) a where
  mapping _ (a /\ b) = a

data Snd = Snd

instance mappingSnd :: Mapping Snd (a /\ b) b where
  mapping _ (a /\ b) = b

instance foldingFstUnit :: Folding Fst Unit (a /\ b) a where
  folding _ _ (a /\ b) = a
else instance foldingFst :: Folding Fst acc (a /\ b) (a /\ acc) where
  folding _ acc (a /\ b) = a /\ acc

instance foldingSndUnit :: Folding Snd Unit (a /\ b) b where
  folding _ _ (a /\ b) = b
else instance foldingSnd :: Folding Snd acc (a /\ b) (b /\ acc) where
  folding _ acc (a /\ b) = b /\ acc

