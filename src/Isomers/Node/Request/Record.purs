module Isomers.Node.Request.Record where

import Data.Argonaut (Json) as Argonaut
import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect.Aff (Fiber)
import Isomers.Node.Request.Body (Buff, Json, Str, _buff, _str) as Body
import Isomers.Node.Request.Body (_json)
import Isomers.Request (ClientBody(..), Duplex) as Request
import Isomers.Request.Duplex.Record (body) as Request.Record
import Node.Buffer (Buffer)
import Node.Stream (Readable) as Node.Stream
import Prim.Row (class Cons, class Lacks) as Row
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row (type (+))

buff ∷
  ∀ l r req ri ro.
  IsSymbol l ⇒
  Row.Lacks l r ⇒
  Row.Cons l ArrayBuffer r ri ⇒
  Row.Cons l Buffer r ro ⇒
  SProxy l →
  Request.Duplex
    (Body.Buff + req)
    (Record r)
    (Record ri)
    (Record ro)
buff l = Request.Record.body Body._buff l Request.ArrayBufferBody

json ∷
  ∀ l r req ri ro.
  IsSymbol l ⇒
  Row.Lacks l r ⇒
  Row.Cons l Argonaut.Json r ri ⇒
  Row.Cons l Argonaut.Json r ro ⇒
  SProxy l →
  Request.Duplex
    (Body.Json + req)
    (Record r)
    (Record ri)
    (Record ro)
json l = Request.Record.body _json l Request.JsonBody

_read = SProxy ∷ SProxy "read"

type Read req a
  = ( read ∷ ∀ r. (Node.Stream.Readable () → r) → Fiber r | req )

str ∷
  ∀ l r req ri ro.
  IsSymbol l ⇒
  Row.Lacks l r ⇒
  Row.Cons l String r ri ⇒
  Row.Cons l String r ro ⇒
  SProxy l →
  Request.Duplex
    (Body.Str + req)
    (Record r)
    (Record ri)
    (Record ro)
str l = Request.Record.body Body._str l Request.StringBody
