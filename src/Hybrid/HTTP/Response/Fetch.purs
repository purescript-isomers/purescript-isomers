module Hybrid.HTTP.Response.Fetch where

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Lazy (Lazy)
import Data.Map (Map)
import Web.File.Blob (Blob) as Web.File
import Web.Streams.ReadableStream (ReadableStream) as Web.Streams

newtype Interface aff = Interface
  { arrayBuffer ∷ aff ArrayBuffer -- Effect (Promise ArrayBuffer)
  , blob ∷ aff Web.File.Blob -- Effect (Promise Blob)
  , body ∷ aff (Web.Streams.ReadableStream Uint8Array) -- Effect
  , headers ∷ Lazy (Map String String)
  , ok ∷ Boolean
  , redirected ∷ Boolean
  , status ∷ Int
  , statusText ∷ String
  , text ∷ aff String -- Effect (Promise String)
  , url ∷ String
  }
