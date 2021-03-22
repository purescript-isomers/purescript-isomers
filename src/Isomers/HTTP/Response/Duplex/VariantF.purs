module Isomers.HTTP.Response.Duplex.VariantF where

import Prelude

import Data.Either (Either)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant (class Contractable, contract, expand) as Functor.Variant
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Isomers.HTTP.Response.Duplex.Duplex (Duplex(..))
import Isomers.HTTP.Response.Fetch (Interface) as Fetch
import Isomers.HTTP.Response.Node (Interface) as Node
import Prim.Row (class Union) as Row

-- | Creates a "partial" `Variant` duplex. It won't print additional cases! They should be handled by other duplexes in `alt` chain.
expand ∷ ∀ gt lt mix i o.
  Partial ⇒
  Row.Union lt mix gt ⇒
  Functor.Variant.Contractable gt lt ⇒
  Duplex (VariantF lt i) (VariantF lt o) → Duplex (VariantF gt i) (VariantF gt o)
expand (Duplex enc dec) = Duplex enc' dec'
  where
    enc' ∷ Node.Interface → (VariantF gt i) → Aff Unit
    enc' ni i = do
      case Functor.Variant.contract i of
        Just i' → enc ni i'
        Nothing → pure unit

    dec' ∷ Fetch.Interface → Aff (Either String (VariantF gt o))
    dec' fi = do
      o ← dec fi
      pure $ Functor.Variant.expand <$> o


