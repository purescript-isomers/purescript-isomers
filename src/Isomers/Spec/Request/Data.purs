module Isomers.Spec.Request.Data where

import Prelude

import Data.Lens (Iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, mapping)
import Isomers.Contrib.Heterogeneous (class HMap', hmap')
import Isomers.Contrib.Heterogeneous.Mappings.Record (Delete(..), Get(..), Insert(..)) as Mappings.Record
import Isomers.Contrib.Heterogeneous.Mappings.Tuple (Cons(..), Fst(..), Snd(..)) as Tuple
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (prefix) as Request.Duplex
import Type.Prelude (SProxy)

newtype Data a = Data a

derive instance newtypeData ∷ Newtype (Data a) _

_Data ∷ ∀ a. Iso (Data a) (Data a) a a
_Data = _Newtype

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

tupleMapping ∷
  ∀ a b b'.
  HMap' (DataMapping (Tuple.Cons a)) b b' ⇒
  HMap' (DataMapping Tuple.Snd) b' b ⇒
  HFoldl (DataFolding Tuple.Fst) Unit b' a ⇒
  RequestDuplex' a →
  RequestDuplex' b →
  RequestDuplex' b'
tupleMapping (RequestDuplex aPrt aPrs) (RequestDuplex bPrt bPrs) = RequestDuplex tuplePrinter tupleParser
  where
  tuplePrinter t = aPrt a <> bPrt b
    where
    a = hfoldl (DataFolding Tuple.Fst) unit t

    b = hmap' (DataMapping Tuple.Snd) t

  tupleParser = ado
    a ← aPrs
    b ← bPrs
    in hmap' (DataMapping (Tuple.Cons a)) b

recordInsertMapping ∷
  ∀ a l r r'.
  HMap' (DataMapping (Mappings.Record.Delete l)) r' r ⇒
  HFoldl (DataFolding (Mappings.Record.Get l)) Unit r' a ⇒
  HMap' (DataMapping (Mappings.Record.Insert l a)) r r' ⇒
  SProxy l →
  RequestDuplex' a →
  RequestDuplex' r →
  RequestDuplex' r'
recordInsertMapping l (RequestDuplex aPrt aPrs) (RequestDuplex bPrt bPrs) = RequestDuplex prt prs
  where
  prt t = aPrt a <> bPrt b
    where
    a = hfoldl (DataFolding (Mappings.Record.Get l)) unit t

    b = hmap' (DataMapping (Mappings.Record.Delete l)) t

  prs = ado
    a ← aPrs
    b ← bPrs
    in hmap' (DataMapping (Mappings.Record.Insert l a)) b


newtype PrefixMapping = PrefixMapping String

instance prefixMapping ∷ Mapping PrefixMapping (RequestDuplex a a) (RequestDuplex a a) where
  mapping (PrefixMapping p) v = Request.Duplex.prefix p v

