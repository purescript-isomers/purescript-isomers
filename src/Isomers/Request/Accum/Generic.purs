module Isomers.Request.Accum.Generic where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Isomers.HTTP.Request (Method(..)) as Request
import Isomers.HTTP.Request.Method (toHTTPMethod)
import Isomers.Request.Accum.Type (Accum, prefix)
import Isomers.Request.Accum.Type (method) as Request.Accum.Type
import Isomers.Request.Accum.Variant (empty) as Request.Accum.Variant
import Isomers.Request.Accum.Variant (injInto)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (SProxy, reflectSymbol)

newtype VariantStep
  = VariantStep (∀ body route i o s. IsSymbol s ⇒ SProxy s → Accum body route i o → Accum body route i o)

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
    (SProxy l)
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
    (Accum body route (Variant i) (Variant o)) <= HFoldlVariantStep body route rec (i ∷ # Type) (o ∷ # Type)

instance hfoldlVariantStep ∷
  ( HFoldlWithIndex
        VariantStep
        (Accum body route (Variant ()) (Variant ()))
        rec
        (Accum body route (Variant i) (Variant o))
    ) ⇒
  HFoldlVariantStep body route rec i o

type PrefixRoutes = Boolean

variant ∷ ∀ body vi vo route rec.
  HFoldlVariantStep body route rec vi vo ⇒
  PrefixRoutes →
  rec →
  Accum body route (Variant vi) (Variant vo)
variant prefixRoutes rec = do
  hfoldlWithIndex (VariantStep step) (Request.Accum.Variant.empty ∷ Accum body route _ _) rec
  where
    step ∷ ∀ sr si sb so ss. IsSymbol ss ⇒ SProxy ss → Accum sb sr si so → Accum sb sr si so
    step l = if prefixRoutes
      then
        prefix (reflectSymbol l)
      else
        identity

-- | TODO: I'm not sure if this record based encoding related
-- | stuff should reside here or rather in `Isomers.Spec`.
-- |
-- | It seems that `VariantStep` function is not enough because
-- | we have to restrict symbols which we accept in the
-- | processing function.
data MethodStep = MethodStep

instance mappingMethodStep ∷
  (IsSymbol l, Row.Cons l Unit ms ("DELETE" :: Unit , "POST" :: Unit, "PUT" :: Unit, "GET" ∷ Unit))
  ⇒ MappingWithIndex MethodStep (SProxy l) (Accum body acc i o) (Accum body acc i o) where
  mappingWithIndex _ l v = Request.Accum.Type.method (toHTTPMethod m) v
    where
      m ∷ Request.Method (Variant ("DELETE" :: Unit , "POST" :: Unit, "PUT" :: Unit, "GET" ∷ Unit))
      m = Request.Method (Variant.inj l unit)

byMethod ∷ ∀ body vi vo route rec rec'.
  HMapWithIndex MethodStep rec rec' ⇒
  HFoldlVariantStep body route rec' vi vo ⇒
  rec →
  Accum body route (Variant vi) (Variant vo)
byMethod rec = do
  let
    rec' = hmapWithIndex MethodStep rec
  hfoldlWithIndex (VariantStep (const $ identity)) (Request.Accum.Variant.empty ∷ Accum body route _ _) rec'
