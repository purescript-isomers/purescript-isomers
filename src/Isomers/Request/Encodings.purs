module Isomers.Request.Encodings where

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
  { body :: Maybe ClientBody
  , headers :: Array (HeaderName /\ String)
  , method :: HTTP.Method.Method
  , path :: String
  }

-- | TODO:
-- | Simplify this `body` into a single type parameter. We want to finally
-- | abstract parsing monad away (by probably using `run`) too and this complication
-- | here is unnecessary.
-- |
-- | It seems that interesting approach could be something like `m (Variant body)`.
-- |
-- | --------
-- |
-- | Current solution was a quick and dirty working aproach - it is for sure overcomplicated.
-- |
-- | * We start with a record of `Fiber`s in the `Left` branch.
-- | * When we want to use the body we pass `Proxy` to access the fiber
-- | in the record.
-- | * We build a `Variant` from the fiber and the label.
-- | * We pass the raw result to the parser result.
-- |
-- | * We have `Maybe` around the body `Variant` so we can enforce also
-- |  an empty body requirement and create / parse "GET requests" on
-- | the client. Even when we agree that we can share `String` body accross
-- | the backends we want to rather preserve this `Maybe` as it gives as
-- | a way to express clearly "empty" requirement and build server requests
-- | on the client (in the router)... we can probably do this too by using
-- | `pure ""`... so it is not definitive how it would end up.
-- |
type ServerRequest (body :: # Type) =
  { body :: Either (Effect { | body }) (Maybe (Variant body))
  , headers :: Lazy (Map HeaderName String)
  , httpVersion :: String
  , method :: String
  , path :: String
  }

