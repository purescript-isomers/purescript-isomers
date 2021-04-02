module Isomers.Response.Duplex.Printer where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (stringify) as Argonaut
import Data.Array (cons) as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Isomers.Response.Duplex.Encodings (NodeBody(..))
import Isomers.Response.Duplex.Encodings (NodeBody, ServerResponse) as Response.Duplex.Encodings
import Network.HTTP.Types (HeaderName, Status)
import Network.HTTP.Types (ok200) as Status
import Node.Buffer (fromString) as Node.Buffer
import Node.Encoding (Encoding(..))

newtype Printer = Printer (Response.Duplex.Encodings.ServerResponse → Response.Duplex.Encodings.ServerResponse)

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

body ∷ Response.Duplex.Encodings.NodeBody → Printer
body b = Printer _ { body = Just b }

json ∷ Json → Printer
json = Argonaut.stringify >>> string

string ∷ String → Printer
string str = body $ NodeBuffer $ unsafePerformEffect $ Node.Buffer.fromString str UTF8

defaultResponse ∷ Response.Duplex.Encodings.ServerResponse
defaultResponse =
  { body: Nothing
  , headers: []
  , status: Status.ok200
  }

run ∷ Printer → Response.Duplex.Encodings.ServerResponse
run (Printer prt) = prt defaultResponse
