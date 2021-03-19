module Isomers.HTTP.Response
  ( module Duplex
  , fromJsonDual
  , notFound
  , redirect
  , _ok
  , NotFoundF(..)
  , NotFound
  , OkF(..)
  , Ok
  , RedirectF(..)
  , Redirect
  , Location
  , Response(..)
  , Response'
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor.Variant (FProxy, VariantF, on)
import Data.Functor.Variant (class Contractable, default, inj) as Functor.Variant
import Data.Lazy (force) as Lazy
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Global.Unsafe (unsafeStringify)
import Isomers.HTTP.ContentTypes (_json) as ContentTypes
import Isomers.HTTP.Response.Duplex (Duplex(..), Duplex')
import Isomers.HTTP.Response.Duplex (Duplex(..), Duplex') as Duplex
import Isomers.HTTP.Response.Duplex.VariantF (expand) as Duplex.VariantF
import Isomers.HTTP.Response.Fetch (Interface(..)) as Response.Fetch
import Isomers.HTTP.Response.Node (Interface(..)) as Response.Node
import Node.Encoding (Encoding(..))
import Polyform.Batteries.Json (JsonDecodingError)
import Polyform.Batteries.Json.Duals (Pure) as Json.Duals
import Polyform.Batteries.Json.Parser (dual') as Json.Parser
import Polyform.Validator.Dual.Pure (runSerializer, runValidator) as Polyform.Validator.Dual.Pure
import Prim.Row (class Union) as Row
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (type (+))

foreign import data ArrayBuffer ∷ Type

type Response (res ∷ # Type) a = VariantF res a
type Response' (res ∷ # Type) contentType a = VariantF (Ok contentType + res) a

data OkF (contentType ∷ Symbol) a = OkF a
derive instance functorOkF ∷ Functor (OkF contentType)

type Ok contentType res = (ok ∷ FProxy (OkF contentType) | res)

_ok = SProxy ∷ SProxy "ok"

type Location = String

data RedirectF a = RedirectF Location

derive instance functorRedirectF ∷ Functor RedirectF

_redirect = SProxy ∷ SProxy "redirect"

type Redirect res = (redirect ∷ FProxy RedirectF | res)

ok ∷ ∀ aff content contentType res. IsSymbol contentType ⇒ Monad aff ⇒ SProxy contentType → Duplex' aff content → Duplex' aff (Response' res contentType content)
ok contentType (Duplex encode decode) = Duplex
  (\n@(Response.Node.Interface nodeInterface) v → do
    let
      handle = Functor.Variant.default (pure unit)
        # on _ok \(OkF i) → do
          nodeInterface.setStatusCode 200
          nodeInterface.setStatusMessage "OK"
          nodeInterface.setHeader "Content-Type" (reflectSymbol contentType)
          encode n i
    handle v
  )
  (\f@(Response.Fetch.Interface fetchInterface) → do
      if fetchInterface.ok
        then do
          content ← decode f
          pure $ (Functor.Variant.inj _ok <<< OkF) <$> content

        else pure $ Left "Expecting Ok response"
  )

fromJsonDual ∷ ∀ aff err o.
  Monad aff ⇒
  Json.Duals.Pure (JsonDecodingError + err) o →
  Duplex' aff (Response' () "application/json" o)
fromJsonDual dual = ok ContentTypes._json d
  where
    dual' = Json.Parser.dual' >>> dual
    d = Duplex
      ( \(Response.Node.Interface ni) o → do
          let
            str = Polyform.Validator.Dual.Pure.runSerializer dual' o
          ni.body.writeString UTF8 str
      )
      (\(Response.Fetch.Interface fi) → do
          -- | TODO:
          -- | * Check `Content-Type` before parsing for efficiency.
          -- | * Use either directly here as a result from fi.text
          -- |
          -- ((Right <$> fi.text) `catchError` \err → pure (Left (unsafeStringify err)))
          --   >>= case _ of
          --       Right str → pure $ lmap unsafeStringify <<< un V <<< Polyform.Validator.Dual.Pure.runValidator dual' $ str
          --       Left err → pure (Left err)
          str <- fi.text
          pure $ lmap unsafeStringify <<< un V <<< Polyform.Validator.Dual.Pure.runValidator dual' $ str

      )

data NotFoundF a = NotFoundF String
derive instance functorNotFoundF ∷ Functor NotFoundF

_notFound = SProxy ∷ SProxy "notFound"

type NotFound res = (notFound ∷ FProxy NotFoundF | res)

-- | TODO: Make notFound more configurable
notFound ∷ ∀ aff o res.
  Monad aff ⇒
  Row.Union res (NotFound + ()) (NotFound + res) ⇒
  Functor.Variant.Contractable (NotFound + res) res ⇒
  Duplex' aff (Response res o) →
  Duplex' aff (Response (NotFound + res) o)
notFound others = duplex <|> Duplex.VariantF.expand others
  where
    duplex = Duplex
      (\n@(Response.Node.Interface nodeInterface) v → do
        let
          handle = Functor.Variant.default (pure unit)
            # on _notFound \(NotFoundF msg) → do
              nodeInterface.setStatusCode 404
              nodeInterface.setStatusMessage "Not Found"
              nodeInterface.setHeader "Content-Type" "text/plain"
              nodeInterface.body.writeString UTF8 msg
        handle v
      )
      (\f@(Response.Fetch.Interface fetchInterface) → do
          if fetchInterface.status == 404
            then do
              msg ← fetchInterface.text
              pure $ Right $ Functor.Variant.inj _notFound <<< NotFoundF $ msg
            else pure $ Left "Expecting NotFound response"
      )

redirect ∷ ∀ aff o res.
  Monad aff ⇒
  Row.Union res (Redirect + ()) (Redirect + res) ⇒
  Functor.Variant.Contractable (Redirect + res) res ⇒
  Duplex' aff (Response res o) →
  Duplex' aff (Response (Redirect + res) o)
redirect others = duplex <|> Duplex.VariantF.expand others
  where
  duplex = Duplex
    (\n@(Response.Node.Interface nodeInterface) v → do
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
            Just location → pure $ (Right <<< Functor.Variant.inj _redirect <<< RedirectF) location
            Nothing → pure $ Left "Missing \"Location\" header in redirect response"
          else pure $ Left "Expecting Ok response"
    )
