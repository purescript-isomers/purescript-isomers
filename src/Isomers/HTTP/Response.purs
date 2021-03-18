module Isomers.HTTP.Response
  ( module Duplex
  , _ok
  , OkF(..)
  , Ok
  , RedirectF(..)
  , Redirect
  , Location
  , Response(..)
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Functor.Variant (FProxy, VariantF, on)
import Data.Functor.Variant (default, inj) as Functor.Variant
import Data.Lazy (force) as Lazy
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Isomers.HTTP.Response.Duplex (Duplex(..)) as Duplex
import Isomers.HTTP.Response.Duplex (Duplex(..), Duplex')
import Isomers.HTTP.Response.Fetch (Interface(..)) as Response.Fetch
import Isomers.HTTP.Response.Node (Interface(..)) as Response.Node
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (type (+))

foreign import data ArrayBuffer :: Type

-- | TODO:
-- | We should have here probably something like: Attachment _ | Redirect _ | Response _
-- | At the moment I'm just testing simple 200 scenario here.

-- | * Attachment value indicates whether file was saved or not.
-- |
-- | TODO:
-- | * Experiment with `VariantF` here. We want to probably have
-- | common shape for the response and expand it when turning
-- | into http response.
-- | * Check if we can turn `ResponseDuplex` into something like
-- | `VariantF response String → Node.HTTP.Response`.
-- |

newtype Response response a = Response (VariantF response a)
instance newtypeResponse ∷ Newtype (Response response a) (VariantF response a) where
  wrap = Response
  unwrap (Response v) = v
derive newtype instance functorResponse ∷ Functor (Response response)
derive newtype instance foldableResponse ∷ (Foldable (VariantF response)) ⇒ Foldable (Response response)
derive newtype instance traverseableResponse ∷ (Traversable (VariantF response)) ⇒ Traversable (Response response)

type Response' res contentType a = Response (Ok contentType + res) a

data OkF (contentType ∷ Symbol) a = OkF a
derive instance functorOkF ∷ Functor (OkF contentType)

type Ok contentType res = (ok ∷ FProxy (OkF contentType) | res)

_ok = SProxy ∷ SProxy "ok"

type Location = String

data RedirectF a = RedirectF Location

derive instance functorRedirectF ∷ Functor RedirectF

_redirect = SProxy ∷ SProxy "redirect"

type Redirect res = (redirect ∷ FProxy RedirectF | res)

ok ∷ ∀ aff content contentType res. IsSymbol contentType ⇒ Monad aff ⇒ Duplex' aff content → Duplex' aff (Response' res contentType content)
ok (Duplex encode decode) = Duplex
  (\n@(Response.Node.Interface nodeInterface) (Response v) → do
    let
      handle = Functor.Variant.default (pure unit)
        # on _ok \(OkF i) → do
          nodeInterface.setStatusCode 200
          nodeInterface.setStatusMessage "OK"
          nodeInterface.setHeader "Content-Type" (reflectSymbol (SProxy ∷ SProxy contentType))
          encode n i
    handle v
  )
  (\f@(Response.Fetch.Interface fetchInterface) → do
      if fetchInterface.ok
        then do
          content ← decode f
          pure $ (Response <<< Functor.Variant.inj _ok <<< OkF) <$> content

        else pure $ Left "Expecting Ok response"
  )

redirect ∷ ∀ aff content res. Monad aff ⇒ Duplex' aff (Response (Redirect + res) content)
redirect = Duplex
  (\n@(Response.Node.Interface nodeInterface) (Response v) → do
    let
      handle = Functor.Variant.default (pure unit)
        # on _redirect \(RedirectF location) → do
          -- | TODO: Handle 301 / 302
          nodeInterface.setStatusCode 302
          nodeInterface.setStatusMessage "Found"
          nodeInterface.setHeader "Location" location
    handle v
  )
  (\f@(Response.Fetch.Interface fetchInterface) → do
      if fetchInterface.redirected
        then case Map.lookup "Location" (Lazy.force fetchInterface.headers) of
          Just location → pure $ (Right <<< Response <<< Functor.Variant.inj _redirect <<< RedirectF) location
          Nothing → pure $ Left "Missing \"Location\" header in redirect response"
        else pure $ Left "Expecting Ok response"
  )
