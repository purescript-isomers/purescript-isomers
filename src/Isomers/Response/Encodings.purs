module Isomers.Response.Encodings where

import Prelude

import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Fiber)
import Network.HTTP.Types (Header, HeaderName, Status)
import Node.Buffer.Immutable (ImmutableBuffer) as Buffer.Immutable
import Node.Stream (Readable, Writable) as Node.Stream
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob) as Web.File

type Base body extra =
  { body :: body
  , status :: Status
  | extra
  }

type NodeWriter r = Node.Stream.Writable r -> Effect Unit

newtype UseWritable = UseWritable (forall r. NodeWriter r)

-- | TODO: Move these to the `Isomers.Node.Response.Duplex.Encodings`
-- | We can probably provide some "basic"
-- | building blocks like `Str = (string ∷ String | res)`
-- | so we can provide basic responses for json, html etc.
data NodeBody
  = NodeBuffer Buffer.Immutable.ImmutableBuffer
  | NodeStream (forall r. Node.Stream.Readable r)
  -- | We assume that writer is closing the buffer on its own...
  | NodeWriter UseWritable

nodeWriter :: forall r. NodeWriter r -> NodeBody
nodeWriter f = NodeWriter $ UseWritable (unsafeCoerce f)

-- | TODO: Parametrize by `body` so any other backend can work with this
-- | lib easily. Currently we are tightly cupled to nodejs.
-- |
-- | I'm not able to confirm but it seems that headers value differ between
-- | `fetch` and `node`.
-- | When accessing fetch header we are getting back a "ByteString" value.
-- | I don't want to introduce parsing at this level.
-- | In the case of node we can use `setHeader` or `setHeaders`.
-- | I'm going to use one of these according to the number of values
-- | in the array.
type ServerHeaders = Array Header

type ServerResponse =
  ( Base
      (Maybe NodeBody)
      (headers :: ServerHeaders)
  )

_string = Proxy :: Proxy "string"

type ClientBodyRow =
  ( arrayBuffer :: Fiber ArrayBuffer -- Effect (Promise ArrayBuffer)
  , blob :: Fiber Web.File.Blob -- Effect (Promise Blob)
  -- | Something like this should be possible when we have WritableStream ;-)
  -- , reader ∷ Web.Streams.WritableStream → Aff Unit
  -- | TODO: This was `Eff` originally. I've changed it to `Aff`
  -- | because it was easier.
  , json :: Fiber Json
  -- , stream ∷ Fiber (Web.Streams.ReadableStream Uint8Array)
  , string :: Fiber String -- Effect (Promise String)
  )

type ClientHeaders = Map HeaderName String

newtype ClientResponse = ClientResponse
  ( Base
      { | ClientBodyRow }
      ( headers :: ClientHeaders
      , redirected :: Boolean
      , url :: String
      )
  )

derive instance newtypeClientResponse :: Newtype ClientResponse _
