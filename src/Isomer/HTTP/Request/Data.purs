module Isomer.HTTP.Request.Data where

import Data.Newtype (class Newtype)
import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, mapping)
import Isomer.Contrib.Heterogeneous (class HMap', hmap')

newtype Data a = Data a

derive instance newtypeData ∷ Newtype (Data a) _

-- | Recursive mapping which goes all the way down to the `Data` leaf.
newtype DataMapping f = DataMapping f

instance dataMapping ∷ (Mapping f a a') ⇒ Mapping (DataMapping f) (Data a) (Data a') where
  mapping (DataMapping f) (Data a) = Data (mapping f a)
else instance dataMappingRec ∷ (HMap' (DataMapping f) v v') ⇒ Mapping (DataMapping f) v v' where
  mapping f a = hmap' f a

-- | TODO:
-- | * Do we want these instances?
-- | * Do we want to provide something similar for the folding?
-- | The first instance allows us to use `hmap (DataMaping f)` on `Data` directly..
-- | The second uses inner instance for the mapping.
instance hMapDataMatch ∷ (Mapping f a a') ⇒ HMap (DataMapping f) (Data a) (Data a') where
  hmap (DataMapping f) (Data a) = Data (mapping f a)
else instance hMapDataPass ∷ (HMap' f a a') ⇒ HMap f (Data a) (Data a') where
  hmap f (Data a) = Data (hmap' f a)

data DataFolding f = DataFolding f

instance dataFolding ∷ (Folding f acc a v) ⇒ Folding (DataFolding f) acc (Data a) v where
  folding (DataFolding f) acc (Data a) = folding f acc a
else instance dataFoldingRec ∷ (HFoldl (DataFolding f) acc v v') ⇒ Folding (DataFolding f) acc v v' where
  folding f acc a = hfoldl f acc a
