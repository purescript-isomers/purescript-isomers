module Hybrid.HTTP.Response where

import Prelude
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, traverseDefault)

foreign import data ArrayBuffer :: Type

-- | TODO:
-- | We should have here probably something like: Attachment _ | Redirect _ | Response _
-- | At the moment I'm just testing simple 200 scenario here.
type Location
  = String

-- | * Attachment value indicates whether file was saved or not.
-- |
-- | TODO:
-- | * Experiment with `VariantF` here. We want to probably have
-- | common shape for the response and expand it when turning
-- | into http response.
-- | * Check if we can turn `ResponseCodec` into something like
-- | `VariantF response String → Node.HTTP.Response`.
-- |
data Response content
  = Attachment Boolean
  | Redirect Location
  | Response content

derive instance functorResponse ∷ Functor Response

instance foldableResponse ∷ Foldable Response where
  foldMap f (Attachment _) = mempty
  foldMap f (Redirect _) = mempty
  foldMap f (Response content) = f content
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableResponse ∷ Traversable Response where
  sequence (Attachment arr) = pure $ Attachment arr
  sequence (Redirect loc) = pure $ Redirect loc
  sequence (Response content) = Response <$> content
  traverse f = traverseDefault f
