module Isomers.HTTP.Response.Duplex where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Isomers.HTTP.Response.Fetch (Interface) as Fetch
import Isomers.HTTP.Response.Node (Interface) as Node

-- | TODO:
-- | I don't have time right now to think about a good API which unifies
-- | fetch and `node-http` APIs. I'm even not sure if I'm able to provide
-- | a good API for this...
-- | So I'm just using these node and fetch interfaces directly here
-- | to encode / decode HTTP response.
-- | We can also consider making this a `Polyform.Dual` with
-- | unified interface.
data Duplex aff i o
  = Duplex
    (Node.Interface aff → i → aff Unit)
    -- | TODO: I think that we should accumlate errors
    -- | and use `V` here.
    (Fetch.Interface aff → aff (Either String o))

derive instance functorDuplex ∷ Functor aff ⇒ Functor (Duplex aff i)

instance applyDuplex ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Apply (Duplex aff i) where
  apply (Duplex e1 d1) (Duplex e2 d2) =
    Duplex
      (\n → e1 n <> e2 n)
      (\f → runExceptT $ ExceptT (d1 f) <*> ExceptT (d2 f))

instance applicativeDuplex ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Applicative (Duplex aff i) where
  pure a = Duplex (\_ _ → pure unit) (const $ pure $ Right a)

instance altDuplex ∷ Monad aff ⇒ Alt (Duplex aff i) where
  alt (Duplex e1 d1) (Duplex e2 d2) =
    Duplex
      (\n i → do
        e1 n i
        e2 n i
      )
      (\f → do
        d1 f >>= case _ of
          Left _ → d2 f
          r → pure r
      )

type Duplex' aff a
  = Duplex aff a a

decode ∷ ∀ aff i o. Fetch.Interface aff → Duplex aff i o → aff (Either String o)
decode f (Duplex _ d) = d f

encode ∷ ∀ aff i o. Node.Interface aff → Duplex aff i o → i → aff Unit
encode f (Duplex e _) = e f

