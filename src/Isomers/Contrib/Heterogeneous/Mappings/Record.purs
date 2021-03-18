module Isomers.Contrib.Heterogeneous.Mappings.Record where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class Folding)
import Heterogeneous.Mapping (class Mapping)
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get, insert) as Record
import Type.Prelude (class IsSymbol, SProxy(..))

data Insert (l ∷ Symbol) a = Insert (SProxy l) a

instance mappingInsert ∷ (IsSymbol l, Row.Lacks l r, Row.Cons l a r r') ⇒ Mapping (Insert l a) { | r } { | r' } where
  mapping (Insert l a) r = Record.insert l a r

newtype Delete (l ∷ Symbol) = Delete (SProxy l)

derive instance newtypeDelete ∷ Newtype (Delete l) _

instance mappingDelete ∷ (IsSymbol l, Row.Lacks l r', Row.Cons l a r' r) ⇒ Mapping (Delete l) { | r } { | r' } where
  mapping (Delete l) r = Record.delete l r

newtype Get (l ∷ Symbol) = Get (SProxy l)

instance mappingGet ∷ (IsSymbol l, Row.Cons l a r' r) ⇒ Mapping (Get l) { | r } a where
  mapping (Get l) r = Record.get l r

instance foldingGetUnit ∷ (IsSymbol l, Row.Cons l a r_ r) ⇒ Folding (Get l) Unit { | r } a where
  folding _ _ r = Record.get (SProxy ∷ SProxy l) r
else instance foldingFst ∷ (IsSymbol l, Row.Cons l a r_ r) ⇒ Folding (Get l) acc { | r } (a /\ acc) where
  folding _ acc r = Record.get (SProxy ∷ SProxy l) r /\ acc
