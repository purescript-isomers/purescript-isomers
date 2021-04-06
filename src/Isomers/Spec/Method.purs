module Isomers.Spec.Method where

import Prelude

import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, hmap)
import Isomers.HTTP (Method(..))
import Isomers.Request.Accum.Generic (byMethod) as Request.Accum.Generic
import Isomers.Request.Accum.Generic (class HFoldlVariantStep, MethodStep)
import Isomers.Spec.Record (UnifyRouteType, SubspecBody)
import Isomers.Spec.Type (ResponseMapping, Spec(..), RequestMapping, _RequestMapping, _ResponseMapping)
import Type.Eval (class Eval)
import Type.Prelude (RProxy)

-- | Given a `Method` with a record of specs
-- | we fold it by prefixing with method duplex.
spec ∷
  ∀ body specs resDpls reqDpls reqDpls' route vi vo.
  -- Eval (UnifyRouteType specs) route ⇒
  Eval (SubspecBody specs) (RProxy body) ⇒
  HMap ResponseMapping { | specs } resDpls ⇒
  HMap RequestMapping { | specs } reqDpls ⇒
  HMapWithIndex MethodStep reqDpls reqDpls' ⇒
  HFoldlVariantStep body route reqDpls' vi vo ⇒
  Method { | specs } →
  Spec body route (Variant vi) (Variant vo) resDpls
spec (Method rec) = do
  let
    resDpls = hmap _ResponseMapping rec

    reqDpl = Request.Accum.Generic.byMethod $ Method (hmap _RequestMapping rec)
  Spec { request: reqDpl, response: resDpls }
