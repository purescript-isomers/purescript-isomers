module Hybrid.Api.Client.Fetch where

import Prelude
import Control.Alt (alt, (<|>))
import Control.Apply (lift2)
import Control.Promise (Promise) as Control
import Control.Promise (Promise, toAff, toAffE) as Control.Promise
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Lazy (defer) as Lazy
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant, case_, on)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (fromFoldable, fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import Hybrid.HTTP (Exchange(..), Response) as Hybrid.HTTP
import Hybrid.HTTP.Exchange (FetchError(..))
import Hybrid.HTTP.Method (_get, _post, Method(..))
import Hybrid.HTTP.Response.Fetch (Interface(..)) as Response.Fetch
import Milkis (Method, Response, URL(..), fetch, getMethod, postMethod, redirectManual, text) as Milkis
import Prim.Row (class Cons, class Union) as Row
import Request.Duplex (RequestDuplex')
import Request.Duplex (print) as Request.Duplex
import Type.Equality (class TypeEquals)
import Type.Equality (from, to) as Type.Equalilty
import Type.Equality (to) as TypeEquals
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Fetch (fetch) as Web.Fetch
import Web.Fetch.Headers (fromObject, toArray) as Web.Fetch.Headers
import Web.Fetch.Request (convertOption, defaultUnsafeOptions, unsafeNew) as Web.Fetch.Request
import Web.Fetch.RequestCredentials (RequestCredentials(..)) as Web.Fetch.RequestCredentials
import Web.Fetch.RequestRedirect (RequestRedirect(..), toString) as Web.Fetch.RequestRedirect
import Web.Fetch.Response (arrayBuffer, blob, body, headers, ok, redirected, status, statusText, text, url) as Web.Fetch.Response
import Web.Promise (Promise) as Web.Promise

toAffPromise ∷ ∀ a. Web.Promise.Promise a → Control.Promise.Promise a
toAffPromise = unsafeCoerce

toAff = Control.Promise.toAff <<< toAffPromise

toAffE = Control.Promise.toAffE <<< map toAffPromise

fetch ∷ ∀ req. RequestDuplex' req → req → Aff (Response.Fetch.Interface Aff)
fetch duplex req = do
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

--   raw ← Milkis.fetch
--     fetchImpl
--     (Milkis.URL request'.path)
--     { body: request'.body
--     , method: toMethod request'.method
--     , headers: Object.fromHomogeneous { "accept": "application/json" }
--     , redirect: Milkis.redirectManual
--     }
--   -- response ← Contrib.Milkis.response raw
-- 
--   -- | TODO: This is inefficient. Make this parsing lazy
--   map (note raw) $ do
--     let
--       redirect = pure $ Contrib.Milkis.redirect raw
--       -- | This could read
--       attachment = Contrib.Milkis.attachment raw
--       response = Contrib.Milkis.response raw
--       lAlt = lift2 alt
--     redirect `lAlt` attachment `lAlt` response
-- 
-- exchange ∷ ∀ req. RequestDuplex' req → req → Aff (Hybrid.HTTP.Exchange req String)
-- exchange duplex request = do
--   response ← lmap (FetchError <<< append "Unable to parse response:"  <<< unsafeStringify) <$> fetch duplex request
--   pure $ Hybrid.HTTP.Exchange request (Just $ response)
-- 
-- 
