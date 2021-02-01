module WebRow.Hybrid.Contrib.Data.Record where

import Prelude

import Data.Symbol (class IsSymbol, SProxy)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row as Row
import Record as Record

-- | Ripped directly from heterogeneous `Test.Record` module.
newtype ZipProp r = ZipProp { | r }

instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProp fns) (SProxy sym) a b where
  mappingWithIndex (ZipProp fns) prop = Record.get prop fns

zip :: forall rfns rin rout.
  HMapWithIndex (ZipProp rfns) { | rin } { | rout } =>
  { | rfns } ->
  { | rin  } ->
  { | rout }
zip =
  hmapWithIndex <<< ZipProp

-- testZip :: _
-- testZip =
--   { foo: add 1
--   , bar: Tuple "bar"
--   , baz: \a -> not a
--   }
--   `zipRecord`
--   { foo: 12
--   , bar: 42.0
--   , baz: true
--   }


-- getByTag ∷ ∀ a r v. Variant v → Record r → a
-- getByTag 

