module Hybrid.HTTP.Exchange where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Hybrid.Api.Spec (FetchError)
import Hybrid.HTTP.Response (Response)

newtype Result res = Result (Either FetchError (Response res))
derive instance functorResult ∷ Functor Result

-- | We want to parameterize by the error finally I think.
data Exchange req res
  = Ongoing req
  | Done req (Result res)

derive instance functorExchange ∷ Functor (Exchange req)

instance bifunctorExchange ∷ Bifunctor Exchange where
  bimap f g (Ongoing req) = Ongoing (f req)
  bimap f g (Done req res) = Done (f req) (g <$> res)

exchange ∷ ∀ req res. req → Maybe (Result res) → Exchange req res
exchange req Nothing = Ongoing req
exchange req (Just res) = Done req res
