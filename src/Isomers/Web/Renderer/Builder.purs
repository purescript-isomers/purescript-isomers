module Isomers.Web.Renderer.Builder where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor)
import Control.Apply as Control.Apply
import Data.Argonaut (Json)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (default, on) as Variant
import Isomers.HTTP (ExchangeF) as HTTP
import Isomers.HTTP.ExchangeF (toExchange, request, response) as ExchangeF
import Isomers.Response (Okayish) as Response
import Isomers.Response.Okayish (toVariant) as Response
import Isomers.Response.Okayish.Type (_ok)
import Isomers.Web.Renderer.Types (Renderer(..))
import Polyform.Batteries.Json (FieldMissing)
import Polyform.Batteries.Json.Duals (Base) as Json.Duals
import Polyform.Batteries.Json.Tokenized.Duals (Pure, item) as Json.Tokenized.Duals
import Polyform.Tokenized.Dual ((~))
import Polyform.Tokenized.Dual (Dual(..)) as Polyform.Tokenized.Dual
import Type.Row (type (+))

-- | TODO: Should we drop `resp` and move to something like: `render ∷ Maybe (Either err b) → m doc` ?
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
newtype Builder router req res err a b doc
  = Builder
  ( BuilderBase
      (Compose (Tuple router) (HTTP.ExchangeF req (Response.Okayish res)))
      (Json.Tokenized.Duals.Pure err)
      a
      b
      doc
  )

derive newtype instance functorBuilder ∷ Functor (Builder router req res err a b)

derive newtype instance ixFunctorBuilder ∷ IxFunctor (Builder router req res err)

derive newtype instance ixApplyBuilder ∷ IxApply (Builder router req res err)

derive newtype instance ixApplicativeBuilder ∷ IxApplicative (Builder router req res err)

request :: forall a err  req res router. Builder router req res err a a req
request = Builder $ BuilderBase
  { dual: identity
  , extract: identity
  , render: \(Compose (_ /\ exch)) → ExchangeF.request exch
  }

router :: forall a err req res router. Builder router req res err a a router
router = Builder $ BuilderBase
  { dual: identity
  , extract: identity
  , render: \(Compose (r /\ _)) → r
  }

response :: forall err req res router v. Builder router req res err v v (Maybe (Response.Okayish res v))
response = Builder $ BuilderBase
  { dual: identity
  , extract: identity
  , render: \(Compose (r /\ exch)) → ExchangeF.response exch
  }

-- | TODO: Rename to `body`
content :: forall err req res router v. Builder router req res err v v (Maybe v)
content = Builder $ BuilderBase
  { dual: identity
  , extract: identity
  , render: \(Compose (r /\ exch)) → ExchangeF.response exch >>= unResponse
  }
  where
    unResponse = Response.toVariant >>> Variant.on _ok Just (Variant.default Nothing)

builder ∷
  ∀ a doc err req res router st.
  Json.Duals.Base Identity (FieldMissing + err) Json st →
  Renderer router req (Response.Okayish res st) doc →
  Builder router req res (FieldMissing + err) a (st /\ a) doc
builder dual (Renderer constructor) =
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
          , render:
              constructor
              <<< map ExchangeF.toExchange
              <<< un Compose
              <<< map fst
          }

-- endpoint ∷
--   ∀ doc err req res resRow router.
--   Builder
--     router
--     req
--     resRow
--     ( arrayExpected ∷ Json
--     , endExpected ∷ Json
--     , jsonDecodingError ∷ String
--     | err
--     )
--     Unit
--     res
--     doc →
--   Response.Duplex' (Response JsonMime () res) /\ Renderer httErrs router req resRow doc
-- endpoint b = Response.asJson (d b) /\ r b
--   where
--   r (Builder (BuilderBase { render })) = render <<< Compose
--   d (Builder (BuilderBase { dual })) =
--     Polyform.Tokenized.Dual.unliftUntokenized (dual Json.Tokenized.Duals.end)
--       <<< Validator.Dual.iso List.fromFoldable Array.fromFoldable
--       <<< Json.Duals.array
