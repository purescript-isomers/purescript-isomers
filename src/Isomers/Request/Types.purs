module Isomers.Request.Types where

import Prelude

import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (Method) as HTTP.Method
import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (un)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Isomers.Contrib.Web.Fetch.Request (fromJson) as Contrib.Web.Fetch.Request
import Network.HTTP.Types (HeaderName)
import Web.Fetch.Headers (fromFoldable) as Fetch.Headers
import Web.Fetch.Request (Request) as Fetch
import Web.Fetch.Request (new') as Fetch.Request
import Web.Fetch.RequestBody (empty, fromArrayBuffer, fromString) as Fetch.RequestBody

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

-- | TODO: Handle redirects etc.
toFetchRequest ∷ ClientRequest → Effect Fetch.Request
toFetchRequest r = do
  let
    headers = Fetch.Headers.fromFoldable <<< map (lmap $ un CaseInsensitiveString) $ r.headers
    toRequestBody = case _ of
      ArrayBufferBody buff → Fetch.RequestBody.fromArrayBuffer buff
      JsonBody json → Contrib.Web.Fetch.Request.fromJson json
      StringBody str → Fetch.RequestBody.fromString str

  Fetch.Request.new' r.path
    { body: maybe Fetch.RequestBody.empty toRequestBody r.body
    , headers
    , method: r.method
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

