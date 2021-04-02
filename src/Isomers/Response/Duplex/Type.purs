module Isomers.Response.Duplex.Type where

import Prelude

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Effect.Aff (Aff)
import Isomers.Response.Duplex.Encodings (ClientResponse, ServerResponse) as Response.Duplex.Encodings
import Isomers.Response.Duplex.Parser (Parser, ParsingError)
import Isomers.Response.Duplex.Parser (run) as Parser
import Isomers.Response.Duplex.Printer (Printer)
import Isomers.Response.Duplex.Printer (run) as Printer

-- | `contentType` can encode different things
-- | for different response types but one invariant
-- | should be preserved:
-- |
-- | If a given response has an OK response it
-- | has to be encoded using declared `contentType`
-- |
-- | The content type information is used to
-- | provide appropriate options for `Accept` header
-- | based routing.
-- |
-- |
data Duplex (contentType ∷ Symbol) i o
  = Duplex
    (i → Printer)
    (Parser o)

derive instance functorDuplex ∷ Functor (Duplex ct i)

-- | Given `Apply` and `Profunctor` instances
-- | we can compose smaller duplexes into larger
-- | ones like:
-- |
-- | n :: forall errs i.  Duplex { x :: Status, y :: String | i } { x :: Status, y :: String }
-- | n = { x: _, y: _ } <$> lcmap _.x status <*> lcmap _.y string
-- |
instance applyDuplex ∷ Apply (Duplex ct i) where
  apply (Duplex e1 d1) (Duplex e2 d2) = Duplex
    (\n → e1 n <> e2 n)
    (d1 <*> d2)

instance applicativeDuplex ∷ Applicative (Duplex ct i) where
  pure a = Duplex mempty (pure a)

instance profunctor ∷ Profunctor (Duplex ct) where
  dimap f g (Duplex enc dec) = Duplex (enc <<< f) (g <$> dec)

type Duplex' ct o = Duplex ct o o

decode ∷ ∀ ct i o. Duplex ct i o → Response.Duplex.Encodings.ClientResponse → Aff (Either ParsingError o)
decode (Duplex enc dec) w = Parser.run dec w

encode ∷ ∀ ct i o. Duplex ct i o → i → Response.Duplex.Encodings.ServerResponse
encode (Duplex enc _) = Printer.run <<< enc

