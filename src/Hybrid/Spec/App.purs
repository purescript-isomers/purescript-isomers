module Hybrid.Spec.App where

import Hybrid.Spec.Api (Codecs) as Hybrid.Spec.Api

type Parts request response render =
  { codecs ∷ Hybrid.Spec.Api.Codecs request response
  , render ∷ { | render }
  }

-- | * Do we want to provide a spec constructor which takes api and render as a separate pieces?
-- | * Do we want to expose all constructors which provide nice generic API (prefixing Record / Variant etc.)
-- | here and in Spec.API as well?

-- newtype Spec request response 
