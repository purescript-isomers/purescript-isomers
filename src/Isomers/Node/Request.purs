module Isomers.Node.Request where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lazy (defer)
import Data.Map (fromFoldable) as Map
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (launchSuspendedAff) as Aff
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object
import Isomers.Node.Request.Body (buff, str) as Body
import Isomers.Node.Types (SimpleBody)
import Isomers.Request.Encodings (ServerRequest) as Request.Encodings
import Node.HTTP (Request, httpVersion, requestHeaders, requestMethod, requestURL) as Node.HTTP

fromNodeRequest ∷ Int → Node.HTTP.Request → Request.Encodings.ServerRequest SimpleBody
fromNodeRequest maxBodySize request = do
  let
    body =
      { buff: _
      , str: _
      }
      <$> Aff.launchSuspendedAff (Body.buff maxBodySize request)
      <*> Aff.launchSuspendedAff (Body.str maxBodySize request)
    headers = defer \_ → Map.fromFoldable <<< map (lmap CaseInsensitiveString) <<< (Object.toUnfoldable ∷ Object String → Array (String /\ String)) $ Node.HTTP.requestHeaders request

  { body: Left body
  , headers
  , httpVersion: Node.HTTP.httpVersion request
  , method: Node.HTTP.requestMethod request
  , path: Node.HTTP.requestURL request
  }


