module Isomers.Spec.Method where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class MappingWithIndex, hmap, hmapWithIndex)
import Isomers.HTTP (Method(..))
import Isomers.HTTP.Request (Method(..)) as Request
import Isomers.HTTP.Request.Method (toHTTPMethod)
import Isomers.Request.Accum.Generic (class HFoldlAccumVariant, VariantStep(..))
import Isomers.Request.Accum.Type (Accum)
import Isomers.Request.Accum.Type (method) as Request.Accum.Type
import Isomers.Request.Accum.Variant (empty) as Request.Accum.Variant
import Isomers.Spec.Types (AccumSpec(..))
import Isomers.Spec.Types.Mappings (GetRequest, GetResponse, _GetRequest, _GetResponse)
import Prim.Row (class Cons) as Row
import Type.Prelude (Proxy)

data MethodStep = MethodStep

instance mappingMethodStep ::
  ( IsSymbol l
  , Row.Cons l Unit ms ("DELETE" :: Unit, "POST" :: Unit, "PUT" :: Unit, "GET" :: Unit)
  ) =>
  MappingWithIndex MethodStep (Proxy l) (Accum acc i o) (Accum acc i o) where
  mappingWithIndex _ l v = Request.Accum.Type.method (toHTTPMethod m) v
    where
    m :: Request.Method (Variant ("DELETE" :: Unit, "POST" :: Unit, "PUT" :: Unit, "GET" :: Unit))
    m = Request.Method (Variant.inj l unit)

withMethod
  :: forall vi vo route rec rec'
   . HMapWithIndex MethodStep rec rec'
  => HFoldlAccumVariant route rec' vi vo
  => rec
  -> Accum route (Variant vi) (Variant vo)
withMethod rec = do
  let
    rec' = hmapWithIndex MethodStep rec
  hfoldlWithIndex (VariantStep (const $ identity)) (Request.Accum.Variant.empty :: Accum route _ _) rec'

-- | Given a `Method` with a record of specs
-- | we fold it by prefixing with method duplex.
accumSpec
  :: forall specs resDpls reqDpls reqDpls' route vi vo
   . HMap GetResponse { | specs } resDpls
  => HMap GetRequest { | specs } reqDpls
  => HMapWithIndex MethodStep reqDpls reqDpls'
  => HFoldlAccumVariant route reqDpls' vi vo
  => Method { | specs }
  -> AccumSpec route (Variant vi) (Variant vo) resDpls
accumSpec (Method rec) = do
  let
    resDpls = hmap _GetResponse rec

    reqDpl = withMethod (Method ((hmap _GetRequest rec) :: reqDpls))
  AccumSpec { request: reqDpl, response: resDpls }

