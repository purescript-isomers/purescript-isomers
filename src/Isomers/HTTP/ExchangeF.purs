module Isomers.HTTP.ExchangeF where

import Prelude

import Data.Foldable (class Foldable)
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Isomers.HTTP.Exchange (Exchange)

newtype ExchangeF errs req res a = ExchangeF (Compose (Exchange errs req) res a)

fromExchange ∷ ∀ a errs req res. Exchange errs req (res a) → ExchangeF errs req res a
fromExchange ex = ExchangeF (Compose ex)

toExchange ∷ ∀ a errs req res. ExchangeF errs req res a → Exchange errs req (res a)
toExchange (ExchangeF (Compose ex)) = ex

derive instance newtypeExchangeF ∷ Newtype (ExchangeF errs req res a) _
derive newtype instance functorEchangeF ∷ Functor res ⇒ Functor (ExchangeF errs req res)
derive newtype instance foldableExchange ∷ Foldable res ⇒ Foldable (ExchangeF errs req res)
derive newtype instance traversableExchange ∷ Traversable res ⇒ Traversable (ExchangeF errs req res)

