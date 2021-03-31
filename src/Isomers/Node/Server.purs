module Isomers.Node.Server where

import Prelude

import Control.Monad.Except (catchError)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (message) as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (error) as Console
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Isomers.Node (Root)
import Isomers.Node.Request (fromNodeRequest)
import Isomers.Node.Response (writeNodeResponse)
import Isomers.Node.Types (Root)
import Isomers.Request (ServerRequest)
import Isomers.Request (parse) as Request
import Isomers.Response (Duplex, ServerResponse, print) as Response
import Isomers.Response (ServerResponse)
import Isomers.Server (RouterStep(..), RoutingError(..), ServerResponseWrapper(..))
import Isomers.Server (router) as Server
import Isomers.Spec (Spec(..))
import Node.HTTP (ListenOptions, Request, Response, close, createServer, listen, responseAsStream, setStatusCode, setStatusMessage) as Node.HTTP
import Node.Stream (end) as Node.Stream
import Prim.Row (class Cons) as Row
import Record (get) as Record

router ∷
  ∀ body handlers request resCodecs.
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit (Variant request) (Aff ServerResponseWrapper) ⇒
  Root (Variant request) { | resCodecs } →
  { | handlers } →
  Node.HTTP.Request →
  Node.HTTP.Response →
  Aff Unit -- (Either RoutingError ServerResponse)
router spec handlers nreq nres = do
  let
    maxBodySize = 1000000
    req = fromNodeRequest maxBodySize nreq
  Server.router spec handlers req >>= case _ of
    Right res → do
      log "REQUEST"
      traceM nreq
      traceM req
      liftEffect $ writeNodeResponse res nres
    Left err → liftEffect $ do
      Node.HTTP.setStatusCode nres 404
      Node.HTTP.setStatusMessage nres "Not Found"
      let
        stream = Node.HTTP.responseAsStream nres
      -- void $ Stream.write stream body $ pure unit
      void $ Node.Stream.end stream $ pure unit

handleRequest router nreq nres = launchAff_ do
  router nreq nres `catchError` \err → do
    liftEffect $ do
      Console.error $ Aff.message err
      Node.HTTP.setStatusCode nres 500
      Node.HTTP.setStatusMessage nres "Internal Server Error"

type ServerM = Effect (Effect Unit → Effect Unit)

-- | Given a `ListenOptions` object, a function mapping `Request` to
-- | `ResponseM`, and a `ServerM` containing effects to run on boot, creates and
-- | runs a HTTPure server without SSL.
serve ::
  ∀ body handlers request resCodecs.
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit (Variant request) (Aff ServerResponseWrapper) ⇒
  Root (Variant request) { | resCodecs } →
  { | handlers } →
  Node.HTTP.ListenOptions →
  Effect Unit →
  ServerM
serve spec handlers options onStarted = do
  let
    r = router spec handlers
  server ← Node.HTTP.createServer (handleRequest r)
  Node.HTTP.listen server options onStarted
  pure $ Node.HTTP.close server
