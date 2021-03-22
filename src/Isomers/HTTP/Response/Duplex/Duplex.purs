module Isomers.HTTP.Response.Duplex.Duplex where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Effect.Aff (Aff)
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
data Duplex i o
  = Duplex
    (Node.Interface → i → Aff Unit)
    -- | TODO: I think that we should accumlate errors
    -- | and use `V` here.
    (Fetch.Interface → Aff (Either String o))

derive instance functorDuplex ∷ Functor (Duplex i)

instance applyDuplex ∷ Apply (Duplex i) where
  apply (Duplex e1 d1) (Duplex e2 d2) =
    Duplex
      (\n → e1 n <> e2 n)
      (\f → runExceptT $ ExceptT (d1 f) <*> ExceptT (d2 f))

instance applicativeDuplex ∷ Applicative (Duplex i) where
  pure a = Duplex (\_ _ → pure unit) (const $ pure $ Right a)

type Duplex' a = Duplex a a

decode ∷ ∀ i o. Fetch.Interface → Duplex i o → Aff (Either String o)
decode f (Duplex _ d) = d f

encode ∷ ∀ i o. Node.Interface → Duplex i o → i → Aff Unit
encode f (Duplex e _) = e f

