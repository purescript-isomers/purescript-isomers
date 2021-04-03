module Isomers.Contrib.Heterogeneous.Filtering where

import Prelude

import Heterogeneous.Mapping (class HMap, class Mapping, ConstMapping(..))
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..), HNothing)
import Prim.Boolean (False, True)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, Cons, Nil) as RL
import Prim.RowList (kind RowList)
import Record (get) as Record
import Record.Builder (Builder) as Record
import Record.Builder (build) as Builder
import Record.Builder (build, insert) as Record.Builder
import Type.Data.Boolean (BProxy(..))
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

-- | Should we express these as folds?
class Filtering f a b | f a → b where
  filtering ∷ f → a → BProxy b

class FilteringWithIndex f i a b | f i a → b where
  filteringWithIndex ∷ f → i → a → BProxy b

class HFilter f t t' | f t → t'  where
  hfilter ∷ f → t → t'

class HFilterWithIndex f t t' | f t → t'  where
  hfilterWithIndex ∷ f → t → t'

instance constFiltering ::
  Filtering f a b =>
  FilteringWithIndex (ConstMapping f) ix a b
  where
  filteringWithIndex (ConstMapping f) _ = filtering f

class OptApply b f x y |  b f x → y where
  optApply ∷ BProxy b → f → x → y

instance optApplyFalse ∷ OptApply False f x x where
  optApply _ _ x = x

instance optApplyTrue ∷ OptApply True (x → y) x y where
  optApply _ f x = f x

instance hfilterRecord ::
  ( RL.RowToList rin rl
  , FilterRecordWithIndex rl rin (ConstMapping fn) () rout
  ) =>
  HFilter fn { | rin } { | rout }
  where
  hfilter fn rec =
    Record.Builder.build
    (filterRecordWithIndexBuilder (RLProxy :: RLProxy rl) rec (ConstMapping fn))
    {}

instance hfilterWithIndexRecord ::
  ( RL.RowToList rin rl
  , FilterRecordWithIndex rl rin fn () rout
  ) =>
  HFilterWithIndex fn { | rin } { | rout }
  where
  hfilterWithIndex fn rec =
    Builder.build (filterRecordWithIndexBuilder (RLProxy :: RLProxy rl) rec fn) {}

class FilterRecordWithIndex (xs ∷ RowList) rxs f (as ∷ # Type) (bs ∷ # Type) | xs → rxs, xs f → bs, xs → as where
  filterRecordWithIndexBuilder ∷ RLProxy xs → { | rxs } → f → Record.Builder { | as } { | bs }

instance filterRecordWithIndexCons ::
  ( IsSymbol sym
  , FilteringWithIndex f (SProxy sym) a keep
  , FilterRecordWithIndex rest rec f as bs
  , Row.Cons sym a rec_ rec
  , Row.Cons sym a bs bs'
  , Row.Lacks sym bs
  , OptApply
      keep
      (Record.Builder { | as } { | bs } → Record.Builder { | as } { | bs' })
      (Record.Builder { | as } { | bs })
      (Record.Builder { | as } { | final })
  ) =>
  FilterRecordWithIndex (RL.Cons sym a rest) rec f as final
  where
  filterRecordWithIndexBuilder _ rec f =
    optApply (filteringWithIndex f prop a) g rest
    where
    prop = SProxy ∷ SProxy sym
    a = Record.get prop rec
    bld = Record.Builder.insert prop a ∷ Record.Builder { | bs } { | bs' }

    rest ∷ Record.Builder { | as } { | bs }
    rest = filterRecordWithIndexBuilder (RLProxy ∷ RLProxy rest) rec f

    g ∷ Record.Builder { | as } { | bs } → Record.Builder { | as } { | bs' }
    g = compose bld

instance filterRecordWithIndexNil ∷ FilterRecordWithIndex RL.Nil rec fn as as where
  filterRecordWithIndexBuilder _ _ _ = identity

data CatMaybes = CatMaybes

instance catMaybesNothing ∷ Filtering CatMaybes HNothing False where
  filtering _ _ = BProxy ∷ BProxy False

instance catMaybesJust ∷ Filtering CatMaybes (HJust a) True where
  filtering _ _ = BProxy ∷ BProxy True

-- instance unwrapJustMapping ∷ Mapping CatMaybes (HJust a) a where
--   mapping _ (HJust a) = a
-- 
-- class (HMap CatMaybes t' t'', HFilter CatMaybes t t') ⇐ FilterMap fn t t' t'' | fn t → t' t'' where
--   filterMap ∷ fn → t → t'


-- instance filterMap 
--   = hmap CatMaybes <<< hfilter CatMaybes
-- 
-- x :: forall t10 t3 t4. HMap CatMaybes t4 t3 => HFilter CatMaybes t10 t4 => t10 -> t3
-- x = hmap CatMaybes <<< hfilter CatMaybes
