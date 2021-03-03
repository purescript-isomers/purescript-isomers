module Hybrid.App.Renderer.Builder where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor)
import Control.Apply as Control.Apply
import Data.Argonaut (Json)
import Data.Array (fromFoldable) as Array
import Data.Identity (Identity)
import Data.List (fromFoldable) as List
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
newtype BuilderBase m resp dual a b doc
  = BuilderBase
  { dual ∷ dual a → dual b
  , extract ∷ b → a
  , render ∷ resp b → m doc
  }

derive instance functorBuilderBase ∷ Functor m ⇒ Functor (BuilderBase m resp dual a b)

instance ixFunctorBuilderBase ∷ Functor m ⇒ IxFunctor (BuilderBase m resp dual) where
  imap = map

instance ixApplyBuilderBase ∷ (Functor resp, Applicative m) ⇒ IxApply (BuilderBase m resp dual) where
  iapply (BuilderBase bf) (BuilderBase ba) =
    BuilderBase
      { dual: ba.dual <<< bf.dual
      , extract: bf.extract <<< ba.extract
      , render:
          \cb → ado
            a2f ← bf.render (Control.Apply.map ba.extract cb)
            a ← ba.render cb
            in
              a2f a
      }

instance ixApplicativeBuilderBase ∷ (Functor resp, Applicative m) ⇒ IxApplicative (BuilderBase m resp dual) where
  ipure a =
    BuilderBase
      { dual: identity
      , extract: identity
      , render: const $ pure a
      }

-- | We accumulate parts of the final response content
-- | using `a` and `b` (which is something like `b ∷ value /\ a`).
newtype Builder m req err a b doc
  = Builder
  ( BuilderBase
      m
      (HTTP.Exchange req)
      (Json.Tokenized.Duals.Pure err)
      a
      b
      doc
  )

derive newtype instance functorBuilder ∷ Functor m ⇒ Functor (Builder m req err a b)

derive newtype instance ixFunctorBuilder ∷ Functor m ⇒ IxFunctor (Builder m req err)

derive newtype instance ixApplyBuilder ∷ Applicative m ⇒ IxApply (Builder m req err)

derive newtype instance ixApplicativeBuilder ∷ Applicative m ⇒ IxApplicative (Builder m req err)

builder ∷
  ∀ a doc err m req st.
  Functor m ⇒
  Json.Duals.Base Identity (FieldMissing + err) Json st →
  Renderer req st (m doc) →
  Builder m req (FieldMissing + err) a (st /\ a) doc
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
          , render: constructor <<< map fst
          }

endpoint ∷
  ∀ doc err req res m.
  Builder
    m
    req
    ( arrayExpected ∷ Json
    , endExpected ∷ Json
    , jsonDecodingError ∷ String
    | err
    )
    Unit
    res
    doc →
  ResponseCodec res /\ Renderer req res (m doc)
endpoint b = fromDual (d b) /\ r b
  where
  r (Builder (BuilderBase { render })) = render
  d (Builder (BuilderBase { dual })) =
    Polyform.Tokenized.Dual.unliftUntokenized (dual Json.Tokenized.Duals.end)
      <<< Validator.Dual.iso List.fromFoldable Array.fromFoldable
      <<< Json.Duals.array
      <<< Json.Parser.dual'
