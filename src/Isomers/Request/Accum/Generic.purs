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

newtype VariantStep = VariantStep
  (forall route i o s. IsSymbol s => Proxy s -> Accum route i o -> Accum route i o)

-- | Fold over a record with duplexes and build duplex
-- | for a variant.
instance foldingVariantStepAccum ::
  ( Row.Union vo vo_ vo'
  , Row.Cons l o vo vo'
  , Row.Cons l i vi vi'
  , IsSymbol l
  ) =>
  FoldingWithIndex
    VariantStep
    (Proxy l)
    (Accum route (Variant vi) (Variant vo))
    (Accum route i o)
    (Accum route (Variant vi') (Variant vo')) where
  foldingWithIndex (VariantStep step) l vd d = do
    let
      f = injInto l <<< step l
    f d vd

-- | Just an alias
class
  HFoldlWithIndex
    VariantStep
    (Accum route (Variant ()) (Variant ()))
    rec
    (Accum route (Variant i) (Variant o)) <=
  HFoldlAccumVariant route rec (i :: Row Type) (o :: Row Type)

instance hfoldlVariantStep ::
  ( HFoldlWithIndex
      VariantStep
      (Accum route (Variant ()) (Variant ()))
      rec
      (Accum route (Variant i) (Variant o))
  ) =>
  HFoldlAccumVariant route rec i o

type PrefixRoutes = Boolean

variant
  :: forall vi vo route rec
   . HFoldlAccumVariant route rec vi vo
  => PrefixRoutes
  -> rec
  -> Accum route (Variant vi) (Variant vo)
variant prefixRoutes rec = do
  hfoldlWithIndex (VariantStep step) (Request.Accum.Variant.empty :: Accum route _ _) rec
  where
  step :: forall sr si so ss. IsSymbol ss => Proxy ss -> Accum sr si so -> Accum sr si so
  step l =
    if prefixRoutes then
      prefix (reflectSymbol l)
    else
      identity
