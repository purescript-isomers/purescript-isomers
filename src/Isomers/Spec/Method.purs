module Isomers.Spec.Method where

import Prelude
import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, hmap)
import Isomers.HTTP (Method(..))
import Isomers.Request.Duplex.Generic (class HFoldlVariantStep, MethodStep)
import Isomers.Request.Duplex.Generic (method) as Request.Duplex.Generic
import Isomers.Spec.Record (SubspecInput, SubspecBody)
import Isomers.Spec.Type (ResponseMapping, Spec(..), RequestMapping, _RequestMapping, _ResponseMapping)
import Type.Eval (class Eval)
import Type.Prelude (RProxy)

-- | Given a `Method` with a record of specs
-- | we fold it by prefixing with method duplex.
spec ∷
  ∀ acc body specs resDpls reqDpls reqDpls' vo.
  Eval (SubspecInput specs) acc ⇒
  Eval (SubspecBody specs) (RProxy body) ⇒
  HMap ResponseMapping { | specs } resDpls ⇒
  HMap RequestMapping { | specs } reqDpls ⇒
  HMapWithIndex MethodStep reqDpls reqDpls' ⇒
  HFoldlVariantStep body acc reqDpls' vo vo ⇒
  Method { | specs } →
  Spec body acc (Variant vo) resDpls
spec (Method rec) = do
  let
    resDpls = hmap _ResponseMapping rec

    reqDpl = Request.Duplex.Generic.method $ Method (hmap _RequestMapping rec)
  Spec { request: reqDpl, response: resDpls }
