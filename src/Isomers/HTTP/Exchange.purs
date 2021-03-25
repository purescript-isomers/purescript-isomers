module Isomers.HTTP.Exchange where

import Prelude

import Data.Array (foldMap)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Data.Variant (Variant)

data FetchError = FetchError String

-- | TODO: Do we want to flatten this to wrapper around `req /\ Maybe res`?
-- |  We want to parameterize by the error finally I think.
-- |
-- | The order of `res` and `req` paramaters is dictated by the `Bifunctor` which
-- | we want to have.
data Exchange errs req res
  = Exchange req (Maybe (Either (Variant (fetchError ∷ String | errs)) res))

derive instance functorExchange ∷ Functor (Exchange errs req)

instance bifunctorExchange ∷ Bifunctor (Exchange errs) where
  bimap f g (Exchange req res) = Exchange (f req) (map g <$> res)

instance foldableExchange ∷ Foldable (Exchange errs req) where
  foldMap f (Exchange _ res) = foldMap (foldMap f) res
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableExchange ∷ Traversable (Exchange errs req) where
  sequence (Exchange req res) = Exchange req <$> (traverse sequence) res
  traverse f = traverseDefault f

