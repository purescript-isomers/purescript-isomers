module Hybrid.Api.Client.Fetch where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Apply (lift2)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Foreign.Object (fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import Hybrid.Contrib.Milkis (attachment, redirect, response) as Contrib.Milkis
import Hybrid.HTTP (Exchange(..), Response) as Hybrid.HTTP
import Hybrid.HTTP.Exchange (FetchError(..))
import Milkis (URL(..), fetch, getMethod, postMethod, redirectManual, text) as Milkis
import Milkis.Impl.Window (windowFetch)
import Request.Duplex (RequestDuplex')
import Request.Duplex (print) as Request.Duplex
import Request.Duplex.Types (Method(..))

fetch ∷ ∀ route. RequestDuplex' route → route → Aff (Either _ (Hybrid.HTTP.Response String))
fetch duplex request = do
  let
    request' = Request.Duplex.print duplex request
  raw ← case request'.method of
    Get →
      Milkis.fetch
        windowFetch
        (Milkis.URL request'.path)
        { method: Milkis.getMethod
        , headers: Object.fromHomogeneous { "accept": "application/json" }
        , redirect: Milkis.redirectManual
        }
    Post → do
      Milkis.fetch
        windowFetch
        (Milkis.URL request'.path)
        { body: request'.content
        , headers: Object.fromHomogeneous { "accept": "application/json" }
        , method: Milkis.postMethod
        , redirect: Milkis.redirectManual
        }
  -- response ← Contrib.Milkis.response raw

  -- | TODO: This is inefficient. Make this parsing lazy
  map (note raw) $ do
    let
      redirect = pure $ Contrib.Milkis.redirect raw
      -- | This could read
      attachment = Contrib.Milkis.attachment raw
      response = Contrib.Milkis.response raw
      lAlt = lift2 alt
    redirect `lAlt` attachment `lAlt` response

exchange ∷ ∀ route. RequestDuplex' route → route → Aff (Hybrid.HTTP.Exchange route String)
exchange duplex request = do
  response ← lmap (FetchError <<< append "Unable to parse response:"  <<< unsafeStringify) <$> fetch duplex request
  pure $ Hybrid.HTTP.Exchange request (Just $ response)


