module Isomers.Response.Duplex.Type where

import Prelude

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Effect.Aff (Aff)
import Isomers.HTTP.Response (Web, Node) as HTTP.Response
import Isomers.Response.Duplex.Parser (Parser, ParsingError)
import Isomers.Response.Duplex.Parser (run) as Parser
import Isomers.Response.Duplex.Printer (Printer)
import Isomers.Response.Duplex.Printer (run) as Printer

data Duplex i o
  = Duplex
    (i → Printer)
    (Parser o)

derive instance functorDuplex ∷ Functor (Duplex i)

-- | Given `Apply` and `Profunctor` instances
-- | we can compose smaller duplexes into larger
-- | ones like:
-- |
-- | n :: forall errs i.  Duplex { x :: Status, y :: String | i } { x :: Status, y :: String }
-- | n = { x: _, y: _ } <$> lcmap _.x status <*> lcmap _.y string
-- |
instance applyDuplex ∷ Apply (Duplex i) where
  apply (Duplex e1 d1) (Duplex e2 d2) = Duplex
    (\n → e1 n <> e2 n)
    (d1 <*> d2)

instance applicativeDuplex ∷ Applicative (Duplex i) where
  pure a = Duplex mempty (pure a)

instance profunctor ∷ Profunctor (Duplex) where
  dimap f g (Duplex enc dec) = Duplex (enc <<< f) (g <$> dec)

type Duplex' o = Duplex o o

decode ∷ ∀ i o. Duplex i o → HTTP.Response.Web → Aff (Either ParsingError o)
decode (Duplex enc dec) w = Parser.run dec w

encode ∷ ∀ i o. Duplex i o → i → HTTP.Response.Node
encode (Duplex enc _) = Printer.run <<< enc

