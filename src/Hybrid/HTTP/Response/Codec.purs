module Hybrid.HTTP.Response.Codec where

import Prelude

import Control.Monad.Except (class MonadError, ExceptT(..), catchError, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Global.Unsafe (unsafeStringify)
import Hybrid.HTTP.Response.Fetch (Interface(..)) as Fetch
import Hybrid.HTTP.Response.Node (Interface(..)) as Node
import Node.Encoding (Encoding(..))
import Polyform.Validator.Dual.Pure (Dual, runSerializer, runValidator) as Polyform.Validator.Dual.Pure

-- | TODO:
-- | I don't have time right now to think about a good API which unifies
-- | fetch and `node-http` APIs. I'm even not sure if I'm able to provide
-- | a good API for this...
-- | So I'm just using these node and fetch interfaces directly here
-- | to encode / decode HTTP response.
data Codec aff i o
  = Codec
    (Node.Interface aff → i → aff Unit)
    (Fetch.Interface aff → aff (Either String o))

derive instance functorCodec ∷ Functor aff ⇒ Functor (Codec aff i)
instance applyCodec ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Apply (Codec aff i) where
  apply (Codec e1 d1) (Codec e2 d2) = Codec
    (\n → e1 n <> e2 n)
    (\f → runExceptT $ ExceptT (d1 f) <*> ExceptT (d2 f))
instance applicativeCodec ∷ (Monad aff, Semigroup (aff Unit)) ⇒ Applicative (Codec aff i) where
  pure a = Codec (\_ _ → pure unit) (const $ pure $ Right a)

type Codec' aff a = Codec aff a a

fromJsonDual ∷ ∀ aff err o. MonadError String aff ⇒ Polyform.Validator.Dual.Pure.Dual err String o → Codec' aff o
fromJsonDual dual = Codec
    (\(Node.Interface nodeInterface) o → do
      let
        str = Polyform.Validator.Dual.Pure.runSerializer dual o
      nodeInterface.setHeader "Content-Type" "application/json"
      nodeInterface.body.writeString UTF8 str
    )
    -- | Check "Content-Type" header
    ( \(Fetch.Interface fetchInterface) → do
        ((Right <$> fetchInterface.text) `catchError` \err → pure (Left (unsafeStringify err))) >>= case _ of
          Right str → pure $ lmap unsafeStringify <<< un V <<< Polyform.Validator.Dual.Pure.runValidator dual $ str
          Left err → pure (Left err)
    )

