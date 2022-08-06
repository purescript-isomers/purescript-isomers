module Isomers.Node.Request.Body where

import Prelude

import Control.Monad.Except (throwError)
import Data.Argonaut (Json) as Argonaut
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchSuspendedAff, nonCanceler)
import Effect.Aff (makeAff) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error) as Effect.Exception
import Effect.Ref (new, read, write) as Ref
import Node.Buffer (Buffer)
import Node.Buffer (concat, size, toString) as Buffer
import Node.Encoding (Encoding(..)) as Encoding
import Node.HTTP (Request) as HTTP.Node
import Node.HTTP (Request) as Node.HTTP
import Node.HTTP (requestAsStream) as HTTP
import Node.Stream (Readable)
import Node.Stream (onData, onEnd) as Stream
import Type.Prelude (Proxy(..))

-- | TODO:
-- | * Catching underlining errors `onError`
-- | * Hanling errors?
_buffs :: Int -> Readable () -> Aff (Array Buffer)
_buffs maxBodySize stream = Aff.makeAff \done -> do
  ref <- Ref.new
    { bufs: []
    , size: 0
    }
  Stream.onData stream \buf -> do
    accum <- Ref.read ref
    size <- Buffer.size buf
    let
      size' = accum.size + size
    if size > maxBodySize then throwError (Effect.Exception.error "Body to large")
    else
      Ref.write { bufs: accum.bufs <> [ buf ], size: size' } ref
  Stream.onEnd stream do
    Ref.read ref >>= _.bufs >>> Right >>> done
  pure nonCanceler

buff :: Int -> Readable () -> Aff Buffer
buff maxBodySize stream =
  _buffs maxBodySize stream >>= Buffer.concat >>> liftEffect

str :: Int -> Readable () -> Aff String
str maxBodySize stream =
  buff maxBodySize stream >>= Buffer.toString Encoding.UTF8 >>> liftEffect

