module Isomers.Node.Request where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lazy (defer)
import Data.Map (fromFoldable) as Map
import Data.Newtype (wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (launchSuspendedAff)
import Effect.Aff (launchSuspendedAff) as Aff
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object
import Isomers.Node.Request.Body (buff, str) as Body
import Isomers.Request.Duplex.Parser (runParser) as Request.Duplex.Parser
import Isomers.Request.Duplex.Path (parse) as Request.Duplex.Path
import Node.HTTP (httpVersion, requestHeaders, requestMethod, requestURL) as Node.HTTP
import Node.HTTP (requestHeaders) as HTTP
import Node.HTTP (requestMethod)

runParser maxBodySize request = do
  let
    -- | TODO: put `defer` here when
    -- | `Effect` gains a `Lazy` instance:
    -- | https://github.com/purescript/purescript-control/issues/57
    body = liftEffect $
      { buff: _
      , str: _
      }
      <$> Aff.launchSuspendedAff (Body.buff maxBodySize request)
      <*> Aff.launchSuspendedAff (Body.str maxBodySize request)
    headers = defer \_ → Map.fromFoldable <<< map (lmap CaseInsensitiveString) <<< (Object.toUnfoldable ∷ Object String → Array (String /\ String)) $ Node.HTTP.requestHeaders request
    path = Request.Duplex.Path.parse $ Node.HTTP.requestURL request

  Request.Duplex.Parser.runParser
    { body: Left body
    , headers
    , httpVersion: Node.HTTP.httpVersion request
    , method: Node.HTTP.requestMethod request
    , path
    }


