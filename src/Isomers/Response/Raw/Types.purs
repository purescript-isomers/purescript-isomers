module Isomers.Response.Raw.Types where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverseDefault)
import Data.Tuple.Nested ((/\))
import Isomers.Response.Encodings (ClientHeaders, ServerHeaders) as Encodings
import Isomers.Response.Okayish.Type (Location)
import Network.HTTP.Types (Status) as HTTP.Types
import Network.HTTP.Types (found302, hLocation, ok200)

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
  , headers ∷ Encodings.ServerHeaders
  , status ∷ HTTP.Types.Status
  }

derive instance functorRawServer ∷ Functor RawServer
instance extendRawServer ∷ Extend RawServer where
  extend f h@(RawServer { status, headers }) = do
    let
      body = (f h)
    RawServer { body, headers, status }
instance foldableRawServer ∷ Foldable RawServer where
  foldMap f (RawServer { body }) = f body
  foldl f = foldlDefault f
  foldr f = foldrDefault f
instance traversableRawServer ∷ Traversable RawServer where
  sequence (RawServer { body, headers, status }) = RawServer <<< { body: _, headers, status } <$> body
  traverse f = traverseDefault f

instance comonadRawServer ∷ Comonad RawServer where
  extract (RawServer { body }) = body

serverOk ∷ ∀ body. body → RawServer body
serverOk body = RawServer { body, headers: mempty, status: ok200 }

serverFound ∷ ∀ body. Location → body → RawServer body
serverFound location body = RawServer { body: body, headers: [ hLocation /\ location ], status: found302 }

newtype RawClient body = RawClient
  { body ∷  body
  , headers ∷ Encodings.ClientHeaders
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

