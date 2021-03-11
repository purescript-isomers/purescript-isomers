module Hybrid.Api.Client.Fetch where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Apply (lift2)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, case_, on)
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Foreign.Object (fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import Hybrid.Contrib.Milkis (attachment, redirect, response) as Contrib.Milkis
import Hybrid.Contrib.Milkis (fetchImpl)
import Hybrid.HTTP (Exchange(..), Response) as Hybrid.HTTP
import Hybrid.HTTP.Exchange (FetchError(..))
import Hybrid.HTTP.Request.Method (_get, _post, Method(..))
import Milkis (Method, Response, URL(..), fetch, getMethod, postMethod, redirectManual, text) as Milkis
import Prim.Row (class Cons, class Union) as Row
import Request.Duplex (RequestDuplex')
import Request.Duplex (print) as Request.Duplex
import Type.Equality (class TypeEquals)
import Type.Equality (from, to) as Type.Equalilty
import Type.Equality (to) as TypeEquals
import Unsafe.Coerce (unsafeCoerce)

fetch ∷ ∀ req. RequestDuplex' req → req → Aff (Either Milkis.Response (Hybrid.HTTP.Response String))
fetch duplex req = do
  let
    request' = Request.Duplex.print duplex req

    toMethod ∷ String → Milkis.Method
    toMethod = unsafeCoerce
  raw ← Milkis.fetch
    fetchImpl
    (Milkis.URL request'.path)
    { body: request'.body
    , method: toMethod request'.method
    , headers: Object.fromHomogeneous { "accept": "application/json" }
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

exchange ∷ ∀ req. RequestDuplex' req → req → Aff (Hybrid.HTTP.Exchange req String)
exchange duplex request = do
  response ← lmap (FetchError <<< append "Unable to parse response:"  <<< unsafeStringify) <$> fetch duplex request
  pure $ Hybrid.HTTP.Exchange request (Just $ response)


