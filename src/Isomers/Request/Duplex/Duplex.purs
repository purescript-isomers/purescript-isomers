module Isomers.Request.Duplex.Duplex where

import Prelude

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Request.Duplex (Request)
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Parser (RouteError, prefix, run) as Parser
import Request.Duplex.Printer (RequestPrinter)
import Request.Duplex.Printer (prefix, run) as Printer

data Duplex r i o
  = Duplex (i → RequestPrinter) (RequestParser (r → o))

type Duplex' r a
  = Duplex r a a

derive instance functorRequestDuplex ∷ Functor (Duplex r i)

instance applyRequestDuplex ∷ Apply (Duplex r i) where
  apply (Duplex encl decl) (Duplex encr decr) =
    Duplex
      (append <$> encl <*> encr)
      (map apply decl <*> decr)

instance applicativeRequestDuplex ∷ Applicative (Duplex r i) where
  pure = Duplex (const mempty) <<< pure <<< pure

instance profunctorRequestDuplex ∷ Profunctor (Duplex r) where
  dimap f g (Duplex enc dec) = Duplex (f >>> enc) (map g <$> dec)

parse ∷ ∀ i o r. Duplex r i o → Request → r → Either Parser.RouteError o
parse (Duplex _ dec) req r = ado
  f ← Parser.run dec req
  in f r

parse' ∷ ∀ i o. Duplex {} i o → Request → Either Parser.RouteError o
parse' d req = parse d req {}

print ∷ ∀ i r o. Duplex r i o → i → Request
print (Duplex enc _) = Printer.run <<< enc

prefix :: forall a b r. String -> Duplex r a b -> Duplex r a b
prefix s (Duplex enc dec) = Duplex (Printer.prefix s <<< enc) (Parser.prefix s dec)

