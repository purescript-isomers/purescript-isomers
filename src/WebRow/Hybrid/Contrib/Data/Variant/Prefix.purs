module WebRow.Hybrid.Data.Variant.Prefix where

import Prelude
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix) as String
import Data.Variant (Unvariant(..), Variant)
import Data.Variant (inj, unvariant) as Variant
import Data.Variant.Internal (VariantRep(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import Prim.Symbol (class Append) as Symbol
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (MapWithIndex)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

class PrefixRow (s ∷ Symbol) (rl ∷ RowList) (rout ∷ # Type) | s rl → rout

instance prefixRowNil ∷ PrefixRow s RowList.Nil ()
else instance prefixRowCons ∷
  ( Symbol.Append s l l'
  , PrefixRow s tail rout
  , Row.Lacks l' rout
  , Row.Cons l' a rout rout'
  ) ⇒
  PrefixRow s (RowList.Cons l a tail) rout'

class UnprefixRow (s ∷ Symbol) (rl ∷ RowList) (rout ∷ # Type) | s rl → rout

instance unprefixRowNil ∷ UnprefixRow s RowList.Nil ()
else instance unprefixRowCons ∷
  ( Symbol.Append s l l'
  , UnprefixRow s tail rout
  , Row.Lacks l rout
  , Row.Cons l a rout rout'
  ) ⇒
  UnprefixRow s (RowList.Cons l' a tail) rout'

add ::
  ∀ p r r' rl.
  IsSymbol p ⇒
  RowToList r rl ⇒
  PrefixRow p rl r' ⇒
  SProxy p ->
  Variant r ->
  Variant r'
add p v =
  let
    VariantRep vr = unsafeCoerce v
  in
    unsafeCoerce
      $ VariantRep
          { type: reflectSymbol p <> vr.type, value: vr.value }

remove ::
  ∀ p r r' rl.
  IsSymbol p ⇒
  RowToList r rl ⇒
  UnprefixRow p rl r' ⇒
  SProxy p ->
  Variant r ->
  Variant r'
remove p v =
  let
    VariantRep vr = unsafeCoerce v

    type' = fromMaybe vr.type $ String.stripPrefix (String.Pattern $ reflectSymbol p) vr.type
  in
    unsafeCoerce
      $ VariantRep
          { type: type', value: vr.value }
