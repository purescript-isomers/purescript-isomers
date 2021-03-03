module Hybrid.Api.Server where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Hybrid.HTTP (Exchange) as HTTP
import Hybrid.HTTP.Exchange (fromResponse) as Exchange
import Node.HTTP (Response) as Node.HTTP

-- | TODO: Move to `VariantF` response representation
-- | which will be tide to a given endpoint:
-- | ```purescript
-- | newtype OkF payload = OkF headers payload
-- |
-- | type Ok c res = (ok ∷ FProxy OkF | res)
-- |
-- | newtype RedirectF payload = RedirectF { location ∷ String, permament ∷ Boolean }
-- |
-- | type Redirect res = (redirect ∷ FProxy RedirecF | res)
-- |
-- | newtype AttachmentF payload = AttachmentF { content ∷ ArrayBuffer, fileName ∷ String }
-- |
-- | type Attachment res = (attachment ∷ AttachmentF | res)
-- | ```
type Result res = Either Node.HTTP.Response res

toHTTP ∷ ∀ req res. req → Result res → Maybe (HTTP.Exchange req res)
toHTTP _ (Left raw) = Nothing
toHTTP req (Right res) = Just $ Exchange.fromResponse req res
