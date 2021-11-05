module Isomers.Request.Accum.Generic where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Request.Accum.Type (Accum, prefix)
import Isomers.Request.Accum.Variant (empty) as Request.Accum.Variant
import Isomers.Request.Accum.Variant (injInto)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (Proxy, reflectSymbol)

newtype VariantStep
  = VariantStep (∀ body route i o s. IsSymbol s ⇒ Proxy s → Accum body route i o → Accum body route i o)

-- | Fold over a record with duplexes and build duplex
-- | for a variant.
instance foldingVariantStepAccum ∷
  ( Row.Union vo vo_ vo'
  , Row.Cons l o vo vo'
  , Row.Cons l i vi vi'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex
    VariantStep
    (Proxy l)
    (Accum body route (Variant vi) (Variant vo))
    (Accum body route i o)
    (Accum body route (Variant vi') (Variant vo')) where
  foldingWithIndex (VariantStep step) l vd d = do
    let
      f = injInto l <<< step l
    f d vd

-- | Just an alias
class
  HFoldlWithIndex
    VariantStep
    (Accum body route (Variant ()) (Variant ()))
    rec
    (Accum body route (Variant i) (Variant o)) <= HFoldlAccumVariant body route rec (i ∷ # Type) (o ∷ # Type)

instance hfoldlVariantStep ∷
  ( HFoldlWithIndex
        VariantStep
        (Accum body route (Variant ()) (Variant ()))
        rec
        (Accum body route (Variant i) (Variant o))
    ) ⇒
  HFoldlAccumVariant body route rec i o

type PrefixRoutes = Boolean

variant ∷ ∀ body vi vo route rec.
  HFoldlAccumVariant body route rec vi vo ⇒
  PrefixRoutes →
  rec →
  Accum body route (Variant vi) (Variant vo)
variant prefixRoutes rec = do
  hfoldlWithIndex (VariantStep step) (Request.Accum.Variant.empty ∷ Accum body route _ _) rec
  where
    step ∷ ∀ sr si sb so ss. IsSymbol ss ⇒ Proxy ss → Accum sb sr si so → Accum sb sr si so
    step l = if prefixRoutes
      then
        prefix (reflectSymbol l)
      else
        identity
