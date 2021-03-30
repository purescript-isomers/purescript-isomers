module Isomers.Request.Duplex.Generic where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Request.Duplex.Type (Duplex, prefix)
import Isomers.Request.Duplex.Variant (empty) as Request.Duplex.Variant
import Isomers.Request.Duplex.Variant (injInto)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (SProxy, reflectSymbol)

type PrefixRoutes = Boolean

newtype VariantStep = VariantStep PrefixRoutes

-- | Fold over a record with duplexes and build duplex
-- | for a variant.
instance foldingVariantStepDuplex ∷
  ( Row.Cons l o () lo
  , Row.Union vo lo vo'
  , Row.Cons l o vo vo'
  , Row.Cons l i vi vi'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex VariantStep (SProxy l) (Duplex body r (Variant vi) (Variant vo)) (Duplex body r i o) (Duplex body r (Variant vi') (Variant vo')) where
  foldingWithIndex (VariantStep prefixRoutes) l vd d = do
    let
      f = injInto l <<< if prefixRoutes
        then
          prefix (reflectSymbol l)
        else
          identity
    f d vd

-- | Just an alias
class HFoldlWithIndex VariantStep (Duplex body r (Variant ()) (Variant ())) rec (Duplex body r (Variant i) (Variant o)) <= HFoldlVariantStep body r rec i o
instance hfoldlVariantStep ∷ (HFoldlWithIndex VariantStep (Duplex body r (Variant ()) (Variant ())) rec (Duplex body r (Variant i) (Variant o))) ⇒ HFoldlVariantStep body r rec i o

variant ∷ ∀ body vi vo r rec.
  HFoldlVariantStep body r rec vi vo ⇒
  PrefixRoutes →
  rec →
  Duplex body r (Variant vi) (Variant vo)
variant p rec = do
  hfoldlWithIndex (VariantStep p) (Request.Duplex.Variant.empty ∷ Duplex body r _ _) rec

