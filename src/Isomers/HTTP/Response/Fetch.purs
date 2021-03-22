module Isomers.HTTP.Response.Fetch where

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Lazy (Lazy)
import Data.Map (Map)
import Effect.Aff (Aff)
import Web.File.Blob (Blob) as Web.File
import Web.Streams.ReadableStream (ReadableStream) as Web.Streams

newtype Interface = Interface
  { arrayBuffer ∷ Aff ArrayBuffer -- Effect (Promise ArrayBuffer)
  , blob ∷ Aff Web.File.Blob -- Effect (Promise Blob)
  , body ∷ Aff (Web.Streams.ReadableStream Uint8Array) -- Effect
  , headers ∷ Lazy (Map String String)
  , ok ∷ Boolean
  , redirected ∷ Boolean
  , status ∷ Int
  , statusText ∷ String
  , text ∷ Aff String -- Effect (Promise String)
  , url ∷ String
  }
