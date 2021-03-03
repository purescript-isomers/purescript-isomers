module Hybrid.HTTP.Exchange where

import Prelude

import Data.Array (foldMap)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Hybrid.HTTP.Response (Response(..))

data FetchError
  = FetchError String
  | NotFound String

-- | * Do we want to flatten this to wrapper around `req /\ Maybe res`?
-- | We want to parameterize by the error finally I think.
data Exchange req res
  = Exchange req (Maybe (Either FetchError (Response res)))

derive instance functorExchange ∷ Functor (Exchange req)

instance bifunctorExchange ∷ Bifunctor Exchange where
  bimap f g (Exchange req res) = Exchange (f req) (map (map g) <$> res)

instance foldableExchange ∷ Foldable (Exchange req) where
  foldMap f (Exchange _ res) = foldMap (foldMap (foldMap f)) $ res
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableExchange ∷ Traversable (Exchange req) where
  sequence (Exchange req res) = Exchange req <$> (traverse $ traverse sequence) res
  traverse f = traverseDefault f

fromResponse ∷ ∀ req res. req → res → Exchange req res
fromResponse req res = Exchange req $ Just $ Right $ Response res
