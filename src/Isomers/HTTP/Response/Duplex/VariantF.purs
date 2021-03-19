module Isomers.HTTP.Response.Duplex.VariantF where

import Prelude

import Data.Either (Either)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant (class Contractable, contract, expand) as Functor.Variant
import Data.Maybe (Maybe(..))
import Isomers.HTTP.Response.Duplex (Duplex(..))
import Isomers.HTTP.Response.Fetch (Interface(..)) as Fetch
import Isomers.HTTP.Response.Node (Interface(..)) as Node
import Prim.Row (class Union) as Row

expand ∷ ∀ m gt lt mix i o.
  Row.Union lt mix gt ⇒
  Functor.Variant.Contractable gt lt ⇒
  Monad m ⇒ Duplex m (VariantF lt i) (VariantF lt o) → Duplex m (VariantF gt i) (VariantF gt o)
expand (Duplex enc dec) = Duplex enc' dec'
  where
    enc' ∷ Node.Interface m → (VariantF gt i) → m Unit
    enc' ni i = do
      case Functor.Variant.contract i of
        Just i' → enc ni i'
        Nothing → pure unit

    dec' ∷ Fetch.Interface m → m (Either String (VariantF gt o))
    dec' fi = do
      o ← dec fi
      pure $ Functor.Variant.expand <$> o


