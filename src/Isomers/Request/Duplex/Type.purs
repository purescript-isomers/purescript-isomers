module Isomers.Request.Duplex.Type where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldr)
import Data.HTTP.Method (Method) as Data.HTTP
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..), split) as String
import Effect.Aff (Aff, Fiber)
import Isomers.Request.Duplex.Parser (Parser, ParsingError)
import Isomers.Request.Duplex.Parser (as, body, int, method, prefix, run, take) as Parser
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Printer (method, prefix, put, run) as Printer
import Isomers.Request.Encodings (ClientRequest, ServerRequest)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy)

data Duplex body i o
  = Duplex (i → Printer) (Parser body o)

type Duplex' body a
  = Duplex body a a

derive instance functorRequestDuplex ∷ Functor (Duplex body i)

instance applyRequestDuplex ∷ Apply (Duplex body i) where
  apply (Duplex encl decl) (Duplex encr decr) =
    Duplex
      (append <$> encl <*> encr)
      (decl <*> decr)

instance applicativeRequestDuplex ∷ Applicative (Duplex body i) where
  pure = Duplex (const mempty) <<< pure

instance profunctorRequestDuplex ∷ Profunctor (Duplex body) where
  dimap f g (Duplex enc dec) = Duplex (f >>> enc) (g <$> dec)

parse ∷ ∀ body i o. Duplex body i o → ServerRequest body → Aff (Either ParsingError o)
parse (Duplex _ dec) req = Parser.run dec req

print ∷ ∀ body i o. Duplex body i o → i → ClientRequest
print (Duplex enc _) = Printer.run <<< enc

body ∷ ∀ b body body_ i o. IsSymbol b ⇒ Row.Cons b (Fiber o) body_ body ⇒ SProxy b → (i → Printer) → Duplex body i o
body l prt = Duplex prt (Parser.body l)

root :: forall body i o. Duplex body i o -> Duplex body i o
root = path ""

path :: forall i body o. String -> Duplex body i o -> Duplex body i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

as :: forall body s a b. (a -> s) -> (String -> Either String b) -> Duplex body s String -> Duplex body a b
as f g (Duplex enc dec) = Duplex (enc <<< f) (Parser.as identity g dec)

int :: ∀ body. Duplex' body String -> Duplex' body Int
int = as show Parser.int

prefix ∷ ∀ body i o. String → Duplex body i o → Duplex body i o
prefix s (Duplex enc dec) = Duplex (Printer.prefix s <<< enc) (Parser.prefix s dec)

string :: ∀ body. Duplex' body String -> Duplex' body String
string = as identity pure

segment :: ∀ body. Duplex' body String
segment = Duplex Printer.put Parser.take

withMethod ∷ ∀ body i o. Data.HTTP.Method → Duplex body i o → Duplex body i o
withMethod m (Duplex enc dec) = Duplex (append (Printer.method m) <<< enc) (Parser.method m dec)

