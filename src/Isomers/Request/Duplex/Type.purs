module Isomers.Request.Duplex.Type where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..), split) as String
import Effect.Aff (Aff)
import Isomers.Request.Duplex.Parser (Parser, ParsingError)
import Isomers.Request.Duplex.Parser (prefix, run) as Parser
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Printer (prefix, run) as Printer
import Isomers.Request.Types (ClientRequest, ServerRequest)

data Duplex body r i o
  = Duplex (i → Printer) (Parser body (r → o))

type Duplex' body r a
  = Duplex body r a a

derive instance functorRequestDuplex ∷ Functor (Duplex body r i)

instance applyRequestDuplex ∷ Apply (Duplex body r i) where
  apply (Duplex encl decl) (Duplex encr decr) =
    Duplex
      (append <$> encl <*> encr)
      (map apply decl <*> decr)

instance applicativeRequestDuplex ∷ Applicative (Duplex body r i) where
  pure = Duplex (const mempty) <<< pure <<< pure

instance profunctorRequestDuplex ∷ Profunctor (Duplex body r) where
  dimap f g (Duplex enc dec) = Duplex (f >>> enc) (map g <$> dec)

parse ∷ ∀ body i o r. Duplex body r i o → ServerRequest body → r → Aff (Either ParsingError o)
parse (Duplex _ dec) req r = ado
  f ← Parser.run dec req
  in f <@> r

parse' ∷ ∀ body i o. Duplex body {} i o → ServerRequest body → Aff (Either ParsingError o)
parse' d req = parse d req {}

print ∷ ∀ body i r o. Duplex body r i o → i → ClientRequest
print (Duplex enc _) = Printer.run <<< enc

prefix ∷ ∀ body i o r. String → Duplex body r i o → Duplex body r i o
prefix s (Duplex enc dec) = Duplex (Printer.prefix s <<< enc) (Parser.prefix s dec)

path ∷ ∀ body i o r. String → Duplex body r i o → Duplex body r i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

