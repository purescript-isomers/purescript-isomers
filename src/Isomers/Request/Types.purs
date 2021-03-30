module Isomers.Request.Types where

import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.HTTP.Method (Method) as HTTP.Method
import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Network.HTTP.Types (HeaderName)

data ClientBody
  = ArrayBufferBody ArrayBuffer
  | JsonBody Json
  | StringBody String

type ClientRequest =
  { body ∷ Maybe ClientBody
  , headers ∷ Array (HeaderName /\ String)
  , method ∷ HTTP.Method.Method
  , path :: String
  }

-- | TODO:
-- | I'm not sure how "generic" the current body representation
-- | really is. If you find something simpler please let me know:
-- |
-- | * We start with a record of `Fiber`s in the `Left` branch.
-- | * When we want to use the body we pass `SProxy` to access the fiber
-- | in the record.
-- | * We build a `Variant` from the fiber and the label.
-- | * We pass the raw result to the parser result.
-- |
-- | The wrapping `Effect` would make record build up lazy... in the future :-P
type ServerRequest (body ∷ # Type) =
  { body ∷ Either (Effect { | body }) (Variant body)
  , headers ∷ Lazy (Map HeaderName String)
  , httpVersion ∷ String
  , method :: String
  , path :: String
  }

