module Isomers.Request.Duplex.Type where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldMap, foldr)
import Data.HTTP.Method (Method) as Data.HTTP
import Data.Maybe (Maybe)
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..), split) as String
import Data.Tuple (uncurry)
import Effect.Aff (Aff, Fiber)
import Isomers.Request.Duplex.Parser (Parser, ParsingError)
import Isomers.Request.Duplex.Parser (as, body, flag, int, method, optional, param, params, prefix, rest, run, take) as Parser
import Isomers.Request.Duplex.Path (Params)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Printer (flag, method, param, prefix, put, run) as Printer
import Isomers.Request.Encodings (ClientRequest, ServerRequest)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, Proxy)

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

body ∷ ∀ b body body_ i o. IsSymbol b ⇒ Row.Cons b (Fiber o) body_ body ⇒ Proxy b → (i → Printer) → Duplex body i o
body l prt = Duplex prt (Parser.body l)

root ∷ ∀ body i o. Duplex body i o → Duplex body i o
root = path ""

path ∷ ∀ i body o. String → Duplex body i o → Duplex body i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

-- | `show` is a function used to construct parsing error message.
as ∷ ∀ body i i' o o'. { print ∷ i' → i, parse ∷ o → Either String o', show ∷ o → String } → Duplex body i o → Duplex body i' o'
as { print: prt, parse: prs, show } (Duplex enc dec) = Duplex (enc <<< prt) (Parser.as { show, parse: prs } dec)

as' ∷ ∀ body i i' o o'. Show o ⇒ (i' → i) → (o → Either String o') → Duplex body i o → Duplex body i' o'
as' prt prs = as { print: prt, parse: prs, show: show }

int ∷ ∀ body. Duplex' body String → Duplex' body Int
int = as' show Parser.int

prefix ∷ ∀ body i o. String → Duplex body i o → Duplex body i o
prefix s (Duplex enc dec) = Duplex (Printer.prefix s <<< enc) (Parser.prefix s dec)

flag ∷ ∀ body. String → Duplex body Boolean Boolean
flag n = Duplex (Printer.flag n) (Parser.flag n)

string ∷ ∀ body. Duplex' body String → Duplex' body String
string = as' show pure

segment ∷ ∀ body. Duplex' body String
segment = Duplex Printer.put Parser.take

param ∷ ∀ body. String → Duplex' body String
param p = Duplex (Printer.param p) (Parser.param p)

withMethod ∷ ∀ body i o. Data.HTTP.Method → Duplex body i o → Duplex body i o
withMethod m (Duplex enc dec) = Duplex (append (Printer.method m) <<< enc) (Parser.method m dec)

rest ∷ ∀ body. Duplex' body (Array String)
rest = Duplex (foldMap Printer.put) Parser.rest

params ∷ ∀ body. Duplex body Params Params
params = Duplex (foldMap (uncurry Printer.param)) Parser.params

optional ∷ ∀ body i o. Duplex body i o → Duplex body (Maybe i) (Maybe o)
optional (Duplex enc dec) = Duplex (foldMap enc) (Parser.optional dec)
