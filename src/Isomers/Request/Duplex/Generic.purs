module Isomers.Request.Duplex.Generic where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Isomers.HTTP.Request (Method(..)) as Request
import Isomers.HTTP.Request.Method (toHTTPMethod)
import Isomers.Request.Duplex.Type (Duplex, prefix)
import Isomers.Request.Duplex.Type (withMethod) as Type
import Isomers.Request.Duplex.Variant (empty) as Request.Duplex.Variant
import Isomers.Request.Duplex.Variant (injInto)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (Proxy, reflectSymbol)

newtype VariantStep = VariantStep (forall i o s. IsSymbol s => Proxy s -> Duplex i o -> Duplex i o)

-- | Fold over a record with duplexes and build duplex
-- | for a variant.
instance foldingVariantStepDuplex ::
  ( Row.Cons l o () lo
  , Row.Union vo lo vo'
  , Row.Cons l o vo vo'
  , Row.Cons l i vi vi'
  , IsSymbol l
  ) =>
  FoldingWithIndex VariantStep
    (Proxy l)
    (Duplex (Variant vi) (Variant vo))
    (Duplex i o)
    (Duplex (Variant vi') (Variant vo')) where
  foldingWithIndex (VariantStep step) l vd d = do
    let
      f = injInto l <<< step l
    f d vd

-- | Just an alias
class
  HFoldlWithIndex VariantStep (Duplex (Variant ()) (Variant ())) rec (Duplex (Variant i) (Variant o)) <=
  HFoldlVariantStep rec (i :: Row Type) (o :: Row Type)

instance hfoldlVariantStep ::
  ( HFoldlWithIndex VariantStep (Duplex (Variant ()) (Variant ())) rec (Duplex (Variant i) (Variant o))
  ) =>
  HFoldlVariantStep rec i o

type PrefixRoutes = Boolean

variant
  :: forall vi vo rec
   . HFoldlVariantStep rec vi vo
  => PrefixRoutes
  -> rec
  -> Duplex (Variant vi) (Variant vo)
variant prefixRoutes rec = do
  hfoldlWithIndex (VariantStep step) (Request.Duplex.Variant.empty :: Duplex _ _) rec
  where
  step :: forall si so ss. IsSymbol ss => Proxy ss -> Duplex si so -> Duplex si so
  step l =
    if prefixRoutes then
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

instance mappingMethodStep ::
  ( IsSymbol l
  , Row.Cons l Unit ms ("DELETE" :: Unit, "POST" :: Unit, "PUT" :: Unit, "GET" :: Unit)
  ) =>
  MappingWithIndex MethodStep (Proxy l) (Duplex i o) (Duplex i o) where
  mappingWithIndex _ l v = Type.withMethod (toHTTPMethod m) v
    where
    m :: Request.Method (Variant ("DELETE" :: Unit, "POST" :: Unit, "PUT" :: Unit, "GET" :: Unit))
    m = Request.Method (Variant.inj l unit)

byMethod
  :: forall vi vo rec rec'
   . HMapWithIndex MethodStep rec rec'
  => HFoldlVariantStep rec' vi vo
  => rec
  -> Duplex (Variant vi) (Variant vo)
byMethod rec = do
  let
    rec' = hmapWithIndex MethodStep rec
  hfoldlWithIndex (VariantStep (const $ identity)) (Request.Duplex.Variant.empty :: Duplex _ _) rec'

