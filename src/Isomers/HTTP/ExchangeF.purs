module Isomers.HTTP.ExchangeF where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Isomers.HTTP.Exchange (Exchange(..))

newtype ExchangeF req res a = ExchangeF (Compose (Exchange req) res a)

derive instance newtypeExchangeF ∷ Newtype (ExchangeF req res a) _
derive newtype instance functorEchangeF ∷ Functor res ⇒ Functor (ExchangeF req res)
derive newtype instance foldableExchange ∷ Foldable res ⇒ Foldable (ExchangeF req res)
derive newtype instance traversableExchange ∷ Traversable res ⇒ Traversable (ExchangeF req res)

request ∷ ∀ a req res. ExchangeF req res a → req
request (ExchangeF (Compose (Exchange r _))) = r

response ∷ ∀ a req res. ExchangeF req res a → Maybe (res a)
response (ExchangeF (Compose (Exchange _ (Just (Right r))))) = Just r
response _ = Nothing

fromExchange ∷ ∀ a req res. Exchange req (res a) → ExchangeF req res a
fromExchange ex = ExchangeF (Compose ex)

toExchange ∷ ∀ a req res. ExchangeF req res a → Exchange req (res a)
toExchange (ExchangeF (Compose ex)) = ex

