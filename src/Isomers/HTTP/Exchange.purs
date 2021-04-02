module Isomers.HTTP.Exchange where

import Prelude

import Data.Array (foldMap)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)

newtype Error = Error String

data Exchange req res = Exchange
  req
  (Maybe (Either Error res))

derive instance functorExchange ∷ Functor (Exchange req)

instance bifunctorExchange ∷ Bifunctor Exchange where
  bimap f g (Exchange req res) = Exchange (f req) (map g <$> res)

instance foldableExchange ∷ Foldable (Exchange req) where
  foldMap f (Exchange _ res) = foldMap (foldMap f) res
  foldr accum = foldrDefault accum
  foldl accum = foldlDefault accum

instance traversableExchange ∷ Traversable (Exchange req) where
  sequence (Exchange req res) = Exchange req <$> (traverse sequence) res
  traverse f = traverseDefault f
