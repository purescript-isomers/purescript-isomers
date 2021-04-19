module Isomers.Node.Response where

import Prelude

import Data.Foldable (for_)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Isomers.Response.Encodings (NodeBody(..), ServerResponse) as Encodings
import Network.HTTP.Types (Status) as HTTP.Types
import Node.HTTP (Response, responseAsStream, setHeader, setStatusCode, setStatusMessage) as Node.HTTP
import Node.Stream (end, pipe, write) as Node.Stream

setStatus ∷ Node.HTTP.Response → HTTP.Types.Status → Effect Unit
setStatus res { code, message } = do
  Node.HTTP.setStatusCode res code
  Node.HTTP.setStatusMessage res message

writeNodeResponse ∷ Encodings.ServerResponse → Node.HTTP.Response → Effect Unit
writeNodeResponse sr response = do
  setStatus response sr.status
  for_ sr.headers \(CaseInsensitiveString n /\ v) → do
    Node.HTTP.setHeader response n v
  let
    res = Node.HTTP.responseAsStream response
    -- | TODO: Ensure that this stream ending is correctly used here?
    end = Node.Stream.end res $ pure unit
  for_ sr.body case _ of
    Encodings.NodeBuffer buff → do
      void $ Node.Stream.write res buff $ end
    Encodings.NodeStream readable →
      void $ Node.Stream.pipe readable res
    Encodings.NodeWriter writer → do
      writer res
  -- void $ Node.Stream.end stream $ pure unit
