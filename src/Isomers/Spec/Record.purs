module Isomers.Spec.Record where


import Data.Variant (Variant)
import Heterogeneous.Mapping (class HMap, hmap)
import Isomers.Request.Accum.Generic (class HFoldlAccumVariant)
import Isomers.Request.Accum.Generic (variant) as Request.Accum.Generic
import Isomers.Spec.Types (AccumSpec(..))
import Isomers.Spec.Types.Mappings (GetRequest, GetResponse, _GetRequest, _GetResponse)

type PrefixRoutes = Boolean

-- | 1. We assume here that record fields are already `Spec` values.
-- |
-- | 2. We map over this record of specs to merge them by performing
-- | these steps:
-- |
-- | * Extract request duplexes from specs so we get a record of
-- | duplexes _RequestMapping.
-- |
-- | * Apply `Request.Duplex.Generic.variant` on the result so we
-- | end up with a single `Request.Duplex`.
-- |
-- | * Map over an original record to extract only response duplexes
-- | which is a value which we want to pass to the final spec record.
accumSpec
  :: forall rec reqs res route ivreq ovreq
   . HMap GetResponse { | rec } { | res }
  => HMap GetRequest { | rec } { | reqs }
  => HFoldlAccumVariant route { | reqs } ivreq ovreq
  => PrefixRoutes
  -> { | rec }
  -> AccumSpec route (Variant ivreq) (Variant ovreq) { | res }
accumSpec b r = do
  let
    reqs :: { | reqs }
    reqs = hmap _GetRequest r
  AccumSpec
    { response: hmap _GetResponse r
    , request: Request.Accum.Generic.variant b reqs
    }

