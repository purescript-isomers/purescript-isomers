module Isomers.Response.Duplex.Printer where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (stringify) as Argonaut
import Data.Array (cons) as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Isomers.Response.Encodings (NodeBody(..), ServerResponse) as Encodings
import Network.HTTP.Types (HeaderName, Status)
import Network.HTTP.Types (ok200) as Status
import Node.Buffer (fromString) as Node.Buffer
import Node.Encoding (Encoding(..))
import Node.Stream (Readable) as Node.Stream

newtype Printer = Printer (Encodings.ServerResponse → Encodings.ServerResponse)

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

body ∷ Encodings.NodeBody → Printer
body b = Printer _ { body = Just b }

json ∷ Json → Printer
json = Argonaut.stringify >>> string

stream ∷ (∀ r. Node.Stream.Readable r) → Printer
stream s = body (Encodings.NodeStream s)

string ∷ String → Printer
string str = body $ Encodings.NodeBuffer $ unsafePerformEffect $ Node.Buffer.fromString str UTF8

defaultResponse ∷ Encodings.ServerResponse
defaultResponse =
  { body: Nothing
  , headers: []
  , status: Status.ok200
  }

run ∷ Printer → Encodings.ServerResponse
run (Printer prt) = prt defaultResponse
