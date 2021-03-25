module Isomers.Response.Duplex.Printer where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (stringify) as Argonaut
import Data.Array (cons) as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Isomers.HTTP.Response (Node, NodeBody) as HTTP.Response
import Isomers.HTTP.Response.Interfaces (NodeBody(..))
import Network.HTTP.Types (HeaderName, Status)
import Network.HTTP.Types (ok200) as Status
import Node.Buffer (fromString) as Node.Buffer
import Node.Encoding (Encoding(..))

newtype Printer = Printer (HTTP.Response.Node → HTTP.Response.Node)

derive instance newtypePrinter ∷ Newtype Printer _

instance semigroupPrinter ∷ Semigroup Printer where
  append (Printer f) (Printer g) = Printer (f >>> g)

instance monoidPrinter ∷ Monoid Printer where
  mempty = Printer identity

header ∷ HeaderName → Maybe String → Printer
header name = case _ of
  Just val → Printer \state → state { headers = Array.cons (name /\ val) state.headers }
  Nothing → mempty

reqHeader ∷ HeaderName → String → Printer
reqHeader name val = header name (Just val)

status ∷ Status → Printer
status s = Printer \state → state { status = s }

body ∷ HTTP.Response.NodeBody → Printer
body b = Printer _ { body = Just b }

json ∷ Json → Printer
json = Argonaut.stringify >>> string

string ∷ String → Printer
string str = body $ NodeBuffer $ unsafePerformEffect $ Node.Buffer.fromString str UTF8

defaultResponse ∷ HTTP.Response.Node
defaultResponse =
  { body: Nothing
  , headers: []
  , status: Status.ok200
  }

run ∷ Printer → HTTP.Response.Node
run (Printer prt) = prt defaultResponse
