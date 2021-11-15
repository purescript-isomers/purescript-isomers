module Isomers.Spec.Method where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Heterogeneous.Folding (hfoldlWithIndex)
import Heterogeneous.Mapping (hmap, class HMap, class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Isomers.HTTP (Method(..))
import Isomers.HTTP.Request (Method(..)) as Request
import Isomers.HTTP.Request.Method (toHTTPMethod)
import Isomers.Request.Accum.Generic (class HFoldlAccumVariant, VariantStep(..))
import Isomers.Request.Accum.Type (Accum)
import Isomers.Request.Accum.Type (method) as Request.Accum.Type
import Isomers.Request.Accum.Variant (empty) as Request.Accum.Variant
import Isomers.Spec.Record (UnifyBody)
import Isomers.Spec.Types (AccumSpec(..), GetRequest, GetResponse, _GetRequest, _GetResponse)
import Prim.Row (class Cons) as Row
import Type.Eval (class Eval)
import Type.Prelude (Proxy)

data MethodStep = MethodStep

instance mappingMethodStep ∷
  (IsSymbol l, Row.Cons l Unit ms ("DELETE" :: Unit , "POST" :: Unit, "PUT" :: Unit, "GET" ∷ Unit))
  ⇒ MappingWithIndex MethodStep (Proxy l) (Accum body acc i o) (Accum body acc i o) where
  mappingWithIndex _ l v = Request.Accum.Type.method (toHTTPMethod m) v
    where
      m ∷ Request.Method (Variant ("DELETE" :: Unit , "POST" :: Unit, "PUT" :: Unit, "GET" ∷ Unit))
      m = Request.Method (Variant.inj l unit)

withMethod ∷ ∀ body vi vo route rec rec'.
  HMapWithIndex MethodStep rec rec' ⇒
  HFoldlAccumVariant body route rec' vi vo ⇒
  rec →
  Accum body route (Variant vi) (Variant vo)
withMethod rec = do
  let
    rec' = hmapWithIndex MethodStep rec
  hfoldlWithIndex (VariantStep (const $ identity)) (Request.Accum.Variant.empty ∷ Accum body route _ _) rec'

-- | Given a `Method` with a record of specs
-- | we fold it by prefixing with method duplex.
accumSpec ∷
  ∀ body specs resDpls reqDpls reqDpls' route vi vo.
  Eval (UnifyBody specs) (Proxy body) ⇒
  HMap GetResponse { | specs } resDpls ⇒
  HMap GetRequest { | specs } reqDpls ⇒
  HMapWithIndex MethodStep reqDpls reqDpls' ⇒
  HFoldlAccumVariant body route reqDpls' vi vo ⇒
  Method { | specs } →
  AccumSpec body route (Variant vi) (Variant vo) resDpls
accumSpec (Method rec) = do
  let
    resDpls = hmap _GetResponse rec

    reqDpl = withMethod (Method ((hmap _GetRequest rec) ∷ reqDpls))
  AccumSpec { request: reqDpl, response: resDpls }


