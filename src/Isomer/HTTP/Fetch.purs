module Isomer.HTTP.Fetch where

import Prelude

import Control.Promise (Promise, toAff, toAffE) as Control.Promise
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lazy (defer) as Lazy
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Foreign.Object (fromFoldable) as Object
import Global.Unsafe (unsafeStringify)
import Isomer.HTTP (Exchange(..)) as Isomer.HTTP
import Isomer.HTTP.Exchange (FetchError(..))
import Isomer.HTTP.Response (Response)
import Isomer.HTTP.Response.Duplex (Duplex', decode) as Response
import Isomer.HTTP.Response.Fetch (Interface(..)) as Response.Fetch
import Request.Duplex (RequestDuplex')
import Request.Duplex (print) as Request.Duplex
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Fetch (fetch) as Web.Fetch
import Web.Fetch.Headers (fromObject, toArray) as Web.Fetch.Headers
import Web.Fetch.Request (convertOption, defaultUnsafeOptions, unsafeNew) as Web.Fetch.Request
import Web.Fetch.RequestRedirect (RequestRedirect(..), toString) as Web.Fetch.RequestRedirect
import Web.Fetch.Response (arrayBuffer, blob, body, headers, ok, redirected, status, statusText, text, url) as Web.Fetch.Response
import Web.Promise (Promise) as Web.Promise

toAffPromise ∷ ∀ a. Web.Promise.Promise a → Control.Promise.Promise a
toAffPromise = unsafeCoerce

toAff :: forall t17. Web.Promise.Promise t17 -> Aff t17
toAff = Control.Promise.toAff <<< toAffPromise

toAffE :: forall t7. Effect (Web.Promise.Promise t7) -> Aff t7
toAffE = Control.Promise.toAffE <<< map toAffPromise

fetch ∷ ∀ req. RequestDuplex' req → req → Aff (Either FetchError (Response.Fetch.Interface Aff))
fetch duplex req =
  map Right go `catchError` (pure <<< Left <<< FetchError <<< unsafeStringify)
  where
    go =do
      let
        request' = Request.Duplex.print duplex req

        unsafeFetchOptions =
          Web.Fetch.Request.defaultUnsafeOptions
            { headers = Web.Fetch.Headers.fromObject <<< Object.fromFoldable <<< map (\(CaseInsensitiveString h /\ v) → h /\ v) $ request'.headers
            , method = request'.method
            , body = Web.Fetch.Request.convertOption (SProxy ∷ SProxy "body") request'.body
            , redirect = Web.Fetch.RequestRedirect.toString Web.Fetch.RequestRedirect.Manual
            }
      response ← do
        fetchRequest ← liftEffect $ Web.Fetch.Request.unsafeNew request'.path unsafeFetchOptions
        toAffE $ Web.Fetch.fetch fetchRequest
      pure
        $ Response.Fetch.Interface
            { arrayBuffer: toAffE $ Web.Fetch.Response.arrayBuffer response
            , blob: toAffE $ Web.Fetch.Response.blob response
            , body: liftEffect $ Web.Fetch.Response.body response
            , headers: Lazy.defer \_ → Map.fromFoldable <<< Web.Fetch.Headers.toArray <<< Web.Fetch.Response.headers $ response
            , ok: Web.Fetch.Response.ok response
            , redirected: Web.Fetch.Response.redirected response
            , status: Web.Fetch.Response.status response
            , statusText: Web.Fetch.Response.statusText response
            , text: toAffE $ Web.Fetch.Response.text response
            , url: Web.Fetch.Response.url response
            }

exchange ∷ ∀ a req res. RequestDuplex' req → req → Response.Duplex' Aff (Response res a) → Aff (Isomer.HTTP.Exchange res req a)
exchange requestDuplex request responseDuplex = do
  response ← fetch requestDuplex request >>= case _ of
    Left err → pure $ Left err
    Right i → lmap FetchError <$> Response.decode i responseDuplex
  pure $ Isomer.HTTP.Exchange request (Just $ response)

