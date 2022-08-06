module Isomers.Request.Encodings where

import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.HTTP.Method (Method) as HTTP.Method
import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Network.HTTP.Types (HeaderName)

data ClientBody
  = ArrayBufferBody ArrayBuffer
  | JsonBody Json
  | StringBody String

type ClientRequest =
  { body :: Maybe ClientBody
  , headers :: Array (HeaderName /\ String)
  , method :: HTTP.Method.Method
  , path :: String
  }

-- | I have not decided yet how to handle request body.
-- | Maybe having this opaque type plus PS engine based
-- | coersions wouldn't be actually so bad.
-- |
-- | We can also cover a specific set of backends or representations
-- | directly like:
-- |
-- | ServerRequestBody = NodeJS ReadableStream | Deno ... | Buny
foreign import data ServerRequestBody :: Type

type ServerRequest =
  { body :: Maybe ServerRequestBody
  , headers :: Lazy (Map HeaderName String)
  , httpVersion :: String
  , method :: String
  , path :: String
  }

