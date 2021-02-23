module Hybrid.Response where

import Prelude

import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverseDefault)

-- | TODO:
-- | We should have here probably something like: Attachment _ | Redirect _ | Response _
-- | At the moment I'm just testing simple 200 scenario here.
newtype Response content
  = Response
  { statusCode ∷ Int
  , content ∷ content
  }

derive instance newtypeResponse ∷ Newtype (Response content) _
derive instance functorResponse ∷ Functor Response

instance foldableResponse ∷ Foldable Response where
  foldMap f (Response { statusCode, content }) = f content
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableResponse ∷ Traversable Response where
  sequence (Response { statusCode, content }) = Response <<< { statusCode, content: _ } <$> content
  traverse f = traverseDefault f

