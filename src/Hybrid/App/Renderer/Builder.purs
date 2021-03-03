module Hybrid.App.Renderer.Builder where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor)
import Control.Apply as Control.Apply
import Data.Argonaut (Json)
import Data.Array (fromFoldable) as Array
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.List (fromFoldable) as List
import Data.Newtype (un)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Hybrid.Api.Spec (ResponseCodec, fromDual)
import Hybrid.App.Renderer.Types (Renderer)
import Hybrid.HTTP (Exchange) as HTTP
import Polyform.Batteries.Json (FieldMissing)
import Polyform.Batteries.Json.Duals (Base, array) as Json.Duals
import Polyform.Batteries.Json.Parser (dual') as Json.Parser
import Polyform.Batteries.Json.Tokenized.Duals (Pure, end, item) as Json.Tokenized.Duals
import Polyform.Tokenized.Dual ((~))
import Polyform.Tokenized.Dual (Dual(..), unliftUntokenized) as Polyform.Tokenized.Dual
import Polyform.Validator.Dual (iso) as Validator.Dual
import Type.Row (type (+))

-- | TODO: Should we drop `resp` and move to something like: `render ∷ Maybe (Either err b) → m doc` ?
-- |
newtype BuilderBase resp dual a b doc
  = BuilderBase
  { dual ∷ dual a → dual b
  , extract ∷ b → a
  , render ∷ resp b → doc
  }

derive instance functorBuilderBase ∷ Functor (BuilderBase resp dual a b)

instance ixFunctorBuilderBase ∷ IxFunctor (BuilderBase resp dual) where
  imap = map

instance ixApplyBuilderBase ∷ (Functor resp) ⇒ IxApply (BuilderBase resp dual) where
  iapply (BuilderBase bf) (BuilderBase ba) =
    BuilderBase
      { dual: ba.dual <<< bf.dual
      , extract: bf.extract <<< ba.extract
      , render:
          \cb →
            let
              a2f = bf.render (Control.Apply.map ba.extract cb)
              a = ba.render cb
            in
              a2f a
      }

instance ixApplicativeBuilderBase ∷ (Functor resp) ⇒ IxApplicative (BuilderBase resp dual) where
  ipure a =
    BuilderBase
      { dual: identity
      , extract: identity
      , render: const $ a
      }

-- | We accumulate parts of the final response content
-- | using `a` and `b` (which is something like `b ∷ value /\ a`).
newtype Builder router req err a b doc
  = Builder
  ( BuilderBase
      (Compose (Tuple router) (HTTP.Exchange req))
      (Json.Tokenized.Duals.Pure err)
      a
      b
      doc
  )

derive newtype instance functorBuilder ∷ Functor (Builder router req err a b)

derive newtype instance ixFunctorBuilder ∷ IxFunctor (Builder router req err)

derive newtype instance ixApplyBuilder ∷ IxApply (Builder router req err)

derive newtype instance ixApplicativeBuilder ∷ IxApplicative (Builder router req err)

builder ∷
  ∀ a doc err req router st.
  Json.Duals.Base Identity (FieldMissing + err) Json st →
  Renderer router req st doc →
  Builder router req (FieldMissing + err) a (st /\ a) doc
builder dual constructor =
  let
    dual' ∷ Json.Tokenized.Duals.Pure (FieldMissing + err) a → Json.Tokenized.Duals.Pure (FieldMissing + err) (st /\ a)
    dual' a =
      Polyform.Tokenized.Dual.Dual $ Tuple
        <$> fst
        ~ Json.Tokenized.Duals.item dual
        <*> snd
        ~ a

    extr ∷ st /\ a → a
    extr = snd
  in
    Builder
      $ BuilderBase
          { dual: dual'
          , extract: extr
          , render: constructor <<< un Compose <<< map fst
          }

endpoint ∷
  ∀ doc err req res router.
  Builder
    router
    req
    ( arrayExpected ∷ Json
    , endExpected ∷ Json
    , jsonDecodingError ∷ String
    | err
    )
    Unit
    res
    doc →
  ResponseCodec res /\ Renderer router req res doc
endpoint b = fromDual (d b) /\ r b
  where
  r (Builder (BuilderBase { render })) = render <<< Compose
  d (Builder (BuilderBase { dual })) =
    Polyform.Tokenized.Dual.unliftUntokenized (dual Json.Tokenized.Duals.end)
      <<< Validator.Dual.iso List.fromFoldable Array.fromFoldable
      <<< Json.Duals.array
      <<< Json.Parser.dual'
