module Isomers.Response.Raw.Types where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Isomers.Response.Duplex.Encodings (ClientHeaders, ServerHeaders)
import Network.HTTP.Types (Status) as HTTP.Types

-- | This kind of encoding is useful when we do rendering of the
-- | response into let say HTML". We
-- | can perform rendering on the backend and get proper response
-- | encoding and we can do the same on the frontend and extract
-- | only the document.
-- |
-- | These types are just a dirty prototypes (they should probably
-- | carry a proper homogeneous Variant under the hood).
-- |
newtype RawServer body = RawServer
  { body ∷  body
  , headers ∷ ServerHeaders
  , status ∷ HTTP.Types.Status
  }

derive instance functorRawServer ∷ Functor RawServer
instance extendRawServer ∷ Extend RawServer where
  extend f h@(RawServer { status, headers }) = do
    let
      body = (f h)
    RawServer { body, headers, status }

instance comonadRawServer ∷ Comonad RawServer where
  extract (RawServer { body }) = body

newtype RawClient body = RawClient
  { body ∷  body
  , headers ∷ ClientHeaders
  , status ∷ HTTP.Types.Status
  }

derive instance functorRawClient ∷ Functor RawClient
instance extendRawClient ∷ Extend RawClient where
  extend f h@(RawClient { status, headers }) = do
    let
      body = (f h)
    RawClient { body, headers, status }

instance comonadRawClient ∷ Comonad RawClient where
  extract (RawClient { body }) = body
