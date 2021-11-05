module Isomers.Node.Server where

import Prelude

import Control.Monad.Except (catchError)
import Data.Either (Either(..))
import Data.Variant (Variant)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (message) as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error) as Console
import Heterogeneous.Folding (class HFoldlWithIndex)
import Isomers.Node.Request (fromNodeRequest)
import Isomers.Node.Response (writeNodeResponse)
import Isomers.Node.Types (NodeSimpleRequest)
import Isomers.Server (RouterStep, ServerResponseWrapper)
import Isomers.Server (router) as Server
import Isomers.Spec (Spec)
import Node.HTTP (ListenOptions, Request, Response, close, createServer, listen, responseAsStream, setStatusCode, setStatusMessage) as Node.HTTP
import Node.HTTP (requestHeaders, requestURL)
import Node.Stream (end) as Node.Stream

-- | TODO: this is just an ugly prototype.
-- | We need to move `maxBodySize` to a parameter
-- | and think about error handling etc.
router ∷
  ∀ handlers ireq m oreq resCodecs.
  MonadAff m ⇒
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit oreq (m ServerResponseWrapper) ⇒
  Spec NodeSimpleRequest ireq oreq { | resCodecs } →
  { | handlers } →
  Node.HTTP.Request →
  Node.HTTP.Response →
  m Unit -- (Either RoutingError ServerResponse)
router spec handlers nreq nres = do
  let
    maxBodySize = 1000000

    req = fromNodeRequest maxBodySize nreq
  Server.router spec handlers req
    >>= case _ of
        Right res → do
          liftEffect $ writeNodeResponse res nres
        Left err →
          liftEffect
            $ do
                Node.HTTP.setStatusCode nres 404
                Node.HTTP.setStatusMessage nres "Not Found"
                let
                  stream = Node.HTTP.responseAsStream nres
                -- void $ Stream.write stream body $ pure unit
                void $ Node.Stream.end stream $ pure unit

type ServerM
  = Effect (Effect Unit → Effect Unit)

-- | Given a `ListenOptions` object, a function mapping `Request` to
-- | `ResponseM`, and a `ServerM` containing effects to run on boot, creates and
-- | runs a HTTPure server without SSL.
serve ::
  ∀ handlers ireq m oreq resCodecs.
  MonadAff m ⇒
  HFoldlWithIndex (RouterStep handlers resCodecs) Unit (Variant oreq) (m ServerResponseWrapper) ⇒
  Spec NodeSimpleRequest ireq (Variant oreq) { | resCodecs } →
  { | handlers } →
  (m ~> Aff) →
  Node.HTTP.ListenOptions →
  Effect Unit →
  ServerM
serve spec handlers interpret options onStarted = do
  let
    r = router spec handlers

    handleRequest nreq nres = do
      traceM (requestURL nreq)
      -- traceM (requestHeaders nreq)
      launchAff_ do
        interpret (r nreq nres)
          `catchError`
            \err → do
              traceM "ERROR?"
              liftEffect
                $ do
                    Console.error $ Aff.message err
                    Node.HTTP.setStatusCode nres 500
                    Node.HTTP.setStatusMessage nres "Internal Server Error"
  server ← Node.HTTP.createServer handleRequest
  Node.HTTP.listen server options onStarted
  pure $ Node.HTTP.close server
