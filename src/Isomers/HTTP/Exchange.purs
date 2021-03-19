module Isomers.HTTP.Exchange where

import Prelude

import Data.Array (foldMap)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Functor.Variant (VariantF)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Isomers.HTTP.Response (Response)

data FetchError
  = FetchError String
  | NotFound String

-- | TODO: Do we want to flatten this to wrapper around `req /\ Maybe res`?
-- |  We want to parameterize by the error finally I think.
-- |
-- | The order of `res` and `req` paramaters is dictated by the `Bifunctor` which
-- | we want to have.
data Exchange res req content
  = Exchange req (Maybe (Either FetchError (Response res content)))

derive instance functorExchange ∷ Functor (Exchange res req)

instance bifunctorExchange ∷ Bifunctor (Exchange res) where
  bimap f g (Exchange req res) = Exchange (f req) (map (map g) <$> res)

instance foldableExchange ∷ (Foldable (VariantF res)) ⇒ Foldable (Exchange res req) where
  foldMap f (Exchange _ res) = foldMap (foldMap (foldMap f)) $ res
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableExchange ∷ (Traversable (VariantF res), Foldable (VariantF res)) ⇒ Traversable (Exchange res req) where
  sequence (Exchange req res) = Exchange req <$> (traverse $ traverse sequence) res
  traverse f = traverseDefault f

fromResponse ∷ ∀ req res content. req → VariantF res content → Exchange res req content
fromResponse req res = Exchange req $ Just $ Right $ res
