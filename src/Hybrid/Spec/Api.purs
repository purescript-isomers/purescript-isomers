module Hybrid.Spec.Api where

import Data.Variant (Variant)
import Request.Duplex (RequestDuplex')

type Codecs request response =
  { request ∷ RequestDuplex' (Variant request)
  , response ∷ { | response }
  }

-- | A "magic wrapper" around `Codecs` record which exposes API type using more convenient type signature.
-- | newtype Spec ... = Spec ...
