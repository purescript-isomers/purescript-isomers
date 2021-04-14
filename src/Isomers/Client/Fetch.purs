module Isomers.Client.Fetch where

import Prelude

import Control.Monad.Except (catchError, throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (fromFoldable) as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (un)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Traversable (traverse)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchSuspendedAff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import Isomers.Contrib.Web.Fetch (json) as Contrib.Web.Fetch.Response
import Isomers.Contrib.Web.Fetch.Request (fromJson) as Contrib.Web.Fetch.Request
import Isomers.Contrib.Web.Promise (toAffE) as Contrib.Web.Pomise
import Isomers.Contrib.Web.Promise (toAffE) as Contrib.Web.Promise
import Isomers.Request.Encodings (ClientBody(..), ClientRequest) as Request.Encodings
import Isomers.Response.Encodings (ClientResponse(..))
import Isomers.Response.Encodings (ClientResponse) as Response.Encodings
import Web.Fetch (fetch) as Web.Fetch
import Web.Fetch.Headers (fromFoldable) as Fetch.Headers
import Web.Fetch.Headers (toArray) as Web.Fetch.Headers
import Web.Fetch.Request (Request, new) as Fetch
import Web.Fetch.Request (defaultOptions) as Fetch.Request
import Web.Fetch.RequestBody (empty, fromArrayBuffer, fromString) as Fetch.RequestBody
import Web.Fetch.Response (Response) as Web.Fetch
import Web.Fetch.Response (arrayBuffer, headers, status, statusText, text, url) as Web.Fetch.Response
import Web.Promise (Promise) as Web

foreign import unsafePatch ∷ String

data Scheme = HTTP | HTTPS

type HostInfo = { hostName ∷ String, port ∷ Int, scheme ∷ Scheme }

toFetchRequest ∷ HostInfo → Request.Encodings.ClientRequest → Effect Fetch.Request
toFetchRequest hostInfo c = do
  Fetch.new url opts
  where
    body = fromMaybe Fetch.RequestBody.empty $ c.body <#> case _ of
      Request.Encodings.ArrayBufferBody buff → Fetch.RequestBody.fromArrayBuffer buff
      Request.Encodings.JsonBody j → Contrib.Web.Fetch.Request.fromJson j
      Request.Encodings.StringBody cr → Fetch.RequestBody.fromString cr

    scheme = case hostInfo.scheme of
      HTTP → "http"
      HTTPS → "https"

    url = scheme <> "://" <> hostInfo.hostName <> ":" <> show hostInfo.port <> c.path

    headers = Fetch.Headers.fromFoldable <<< map (lmap (un CaseInsensitiveString)) $ c.headers
    opts = Fetch.Request.defaultOptions { body = body, headers = headers, method = c.method }


fromFetchResponse ∷ Web.Fetch.Response → Effect Response.Encodings.ClientResponse
fromFetchResponse res = do
  let
    headers = Map.fromFoldable <<< map (lmap CaseInsensitiveString) <<< Web.Fetch.Headers.toArray <<< Web.Fetch.Response.headers $ res

    status =
      { code: Web.Fetch.Response.status res
      , message: Web.Fetch.Response.statusText res
      }

    url = Web.Fetch.Response.url res

    toFiber ∷ ∀ a. Effect (Web.Promise a) → Effect (Fiber a)
    toFiber = launchSuspendedAff <<< Contrib.Web.Promise.toAffE
  body ←
    { arrayBuffer: _
    , json: _
    , string: _
    }
      <$> (toFiber $ Web.Fetch.Response.arrayBuffer res)
      <*> (toFiber $ Contrib.Web.Fetch.Response.json res)
      <*> (toFiber $ Web.Fetch.Response.text res)

  pure $ ClientResponse
    { body
    , headers
    , status
    , url
    }

fetch ∷ HostInfo → Request.Encodings.ClientRequest → Aff (Either String Response.Encodings.ClientResponse)
fetch hostInfo req = do
  req' ← liftEffect $ toFetchRequest hostInfo req
  let
    res = Contrib.Web.Pomise.toAffE $ Web.Fetch.fetch req'
  (res >>= fromFetchResponse >>> map Right >>> liftEffect) `catchError` \err → do
    traceM $ unsafeStringify err
    traceM "EXCEPTION?"
    pure $ Left $ unsafeStringify err

    -- ) >>= traverse liftEffect


