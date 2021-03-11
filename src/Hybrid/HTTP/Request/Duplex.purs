module Hybrid.HTTP.Request.Duplex where

import Prelude

import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class Mapping)
import Hybrid.Contrib.Heterogeneous (class HMap', hmap')
import Hybrid.Contrib.Heterogeneous.Mappings.Record (Delete(..), Get(..), Insert(..)) as Mappings.Record
import Hybrid.Contrid.Heterogeneous.Mappings.Tuple (Cons(..), Fst(..), Snd(..)) as Tuple
import Hybrid.HTTP.Request.Data (DataFolding(..), DataMapping(..))
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (prefix) as Request.Duplex
import Type.Prelude (SProxy)

dataTupleCons ∷
  ∀ a b b'.
  HMap' (DataMapping (Tuple.Cons a)) b b' ⇒
  HMap' (DataMapping Tuple.Snd) b' b ⇒
  HFoldl (DataFolding Tuple.Fst) Unit b' a ⇒
  RequestDuplex' a →
  RequestDuplex' b →
  RequestDuplex' b'
dataTupleCons (RequestDuplex aPrt aPrs) (RequestDuplex bPrt bPrs) = RequestDuplex tuplePrinter tupleParser
  where
  tuplePrinter t = aPrt a <> bPrt b
    where
    a = hfoldl (DataFolding Tuple.Fst) unit t

    b = hmap' (DataMapping Tuple.Snd) t

  tupleParser = ado
    a ← aPrs
    b ← bPrs
    in hmap' (DataMapping (Tuple.Cons a)) b

dataRecordInsert ∷
  ∀ a l r r'.
  HMap' (DataMapping (Mappings.Record.Delete l)) r' r ⇒
  HFoldl (DataFolding (Mappings.Record.Get l)) Unit r' a ⇒
  HMap' (DataMapping (Mappings.Record.Insert l a)) r r' ⇒
  SProxy l →
  RequestDuplex' a →
  RequestDuplex' r →
  RequestDuplex' r'
dataRecordInsert l (RequestDuplex aPrt aPrs) (RequestDuplex bPrt bPrs) = RequestDuplex prt prs
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

