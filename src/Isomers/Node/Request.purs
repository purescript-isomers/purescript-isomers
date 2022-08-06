module Isomers.Node.Request where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lazy (defer)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object
import Isomers.Node.Request.Body as Body
import Isomers.Request.Encodings (ServerRequest, ServerRequestBody) as Request.Encodings
import Isomers.Runtime as Runtime
import JS.Unsafe.Stringify (unsafeStringify)
import Node.HTTP (Request, httpVersion, requestAsStream, requestHeaders, requestMethod, requestURL) as Node.HTTP
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

newtype NodeBody = NodeBody
  { maxBodySize :: Int
  , stream :: Stream.Readable ()
  }

toServerBody :: NodeBody -> Request.Encodings.ServerRequestBody
toServerBody = unsafeCoerce

-- | Given a witness that we are on nodejs
-- | I can decode this server request.
fromServerBody :: Runtime.Node -> Request.Encodings.ServerRequestBody -> NodeBody
fromServerBody _ = unsafeCoerce

fromNodeRequest :: Int -> Node.HTTP.Request -> Request.Encodings.ServerRequest
fromNodeRequest maxBodySize request = do
  let
    body = toServerBody $ NodeBody { stream: Node.HTTP.requestAsStream request, maxBodySize }
    headers = defer \_ ->
      Map.fromFoldable <<< map (lmap CaseInsensitiveString) <<<
        (Object.toUnfoldable :: Object String -> Array (String /\ String)) $ Node.HTTP.requestHeaders request
  { body: Just body
  , headers
  , httpVersion: Node.HTTP.httpVersion request
  , method: Node.HTTP.requestMethod request
  , path: Node.HTTP.requestURL request
  }

readBodyString :: NodeBody -> Aff (Either String String)
readBodyString (NodeBody { maxBodySize, stream }) = do
  let
    readBody = Right <$> Body.str maxBodySize stream
  readBody `catchError` \err -> pure $ Left $ unsafeStringify err


