module Isomers.Node.Response where

import Prelude

import Data.Foldable (for_)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Isomers.Response.Duplex.Encodings (ServerResponse, NodeBody(..))
import Network.HTTP.Types (Status) as HTTP.Types
import Node.HTTP (Response, responseAsStream, setHeader, setStatusCode, setStatusMessage) as Node.HTTP
import Node.Stream (end, write) as Node.Stream

setStatus ∷ Node.HTTP.Response → HTTP.Types.Status → Effect Unit
setStatus res { code, message } = do
  Node.HTTP.setStatusCode res code
  Node.HTTP.setStatusMessage res message

writeNodeResponse ∷ ServerResponse → Node.HTTP.Response → Effect Unit
writeNodeResponse sr response = do
  setStatus response sr.status
  for_ sr.headers \(CaseInsensitiveString n /\ v) → do
    traceM n
    traceM v
    Node.HTTP.setHeader response n v
  let
    stream = Node.HTTP.responseAsStream response
  for_ sr.body case _ of
    NodeBuffer buff → do
      void $ Node.Stream.write stream buff $ pure unit
    NodeStream readable → pure unit
    NodeWriter writer → launchAff_ (writer stream)
  void $ Node.Stream.end stream $ pure unit
