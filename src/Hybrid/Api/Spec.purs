module Hybrid.Api.Spec where

import Prelude
import Data.Variant (Variant)
import Request.Duplex (RequestDuplex')

import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Polyform.Validator.Dual.Pure

type Codecs request response =
  { request ∷ RequestDuplex' (Variant request)
  , response ∷ { | response }
  }

newtype ResponseCodec a
  = ResponseCodec
  { decode ∷ String → Maybe a
  , encode ∷ a → String
  }


fromDual ∷ ∀ err o. Polyform.Validator.Dual.Pure.Dual err String o → ResponseCodec o
fromDual dual =
  ResponseCodec
    { decode: hush <<< un V <<< Polyform.Validator.Dual.Pure.runValidator dual
    , encode: Polyform.Validator.Dual.Pure.runSerializer dual
    }

