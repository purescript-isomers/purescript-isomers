module Isomer.HTTP.Response.Duplex where

import Prelude

import Control.Monad.Except (class MonadError, ExceptT(..), catchError, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Global.Unsafe (unsafeStringify)
import Isomer.HTTP.Response.Fetch (Interface(..)) as Fetch
import Isomer.HTTP.Response.Node (Interface(..)) as Node
import Node.Encoding (Encoding(..))
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Polyform.Validator.Dual.Pure

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
    (Fetch.Interface aff → aff (Either String o))

derive instance functorDuplex ∷ Functor aff ⇒ Functor (Duplex aff i)

instance applyDuplex ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Apply (Duplex aff i) where
  apply (Duplex e1 d1) (Duplex e2 d2) =
    Duplex
      (\n → e1 n <> e2 n)
      (\f → runExceptT $ ExceptT (d1 f) <*> ExceptT (d2 f))

instance applicativeDuplex ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Applicative (Duplex aff i) where
  pure a = Duplex (\_ _ → pure unit) (const $ pure $ Right a)

type Duplex' aff a
  = Duplex aff a a

fromJsonDual ∷ ∀ aff err o. MonadError String aff ⇒ Polyform.Validator.Dual.Pure.Dual err String o → Duplex' aff o
fromJsonDual dual =
  Duplex
    ( \(Node.Interface nodeInterface) o → do
        let
          str = Polyform.Validator.Dual.Pure.runSerializer dual o
        nodeInterface.setHeader "Content-Type" "application/json"
        nodeInterface.body.writeString UTF8 str
    )
    -- | Check "Content-Type" header
    ( \(Fetch.Interface fetchInterface) → do
        ((Right <$> fetchInterface.text) `catchError` \err → pure (Left (unsafeStringify err)))
          >>= case _ of
              Right str → pure $ lmap unsafeStringify <<< un V <<< Polyform.Validator.Dual.Pure.runValidator dual $ str
              Left err → pure (Left err)
    )

decode ∷ ∀ aff i o. Fetch.Interface aff → Duplex aff i o → aff (Either String o)
decode f (Duplex _ d) = d f

encode ∷ ∀ aff i o. Node.Interface aff → Duplex aff i o → i → aff Unit
encode f (Duplex e _) = e f

