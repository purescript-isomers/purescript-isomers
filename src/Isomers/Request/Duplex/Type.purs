module Isomers.Request.Duplex.Type where

import Prelude

import Data.Argonaut (Json, jsonParser, stringify)
import Data.Either (Either)
import Data.Foldable (foldMap, foldr)
import Data.HTTP.Method (Method) as Data.HTTP
import Data.Maybe (Maybe)
import Data.Profunctor (class Profunctor)
import Data.String (Pattern(..), split) as String
import Data.Tuple (uncurry)
import Effect.Aff (Aff)
import Isomers.Request.Duplex.Parser (Parser, ParsingError)
import Isomers.Request.Duplex.Parser (as, body, flag, int, method, optional, param, params, prefix, rest, run, take) as Parser
import Isomers.Request.Duplex.Path (Params)
import Isomers.Request.Duplex.Printer (Printer)
import Isomers.Request.Duplex.Printer (body, flag, method, param, prefix, put, run) as Printer
import Isomers.Request.Encodings (ClientBody, ClientRequest, ServerRequest, ServerRequestBody)

data Duplex i o = Duplex (i -> Printer) (Parser o)

type Duplex' a = Duplex a a

derive instance functorRequestDuplex :: Functor (Duplex i)

instance applyRequestDuplex :: Apply (Duplex i) where
  apply (Duplex encl decl) (Duplex encr decr) =
    Duplex
      (append <$> encl <*> encr)
      (decl <*> decr)

instance applicativeRequestDuplex :: Applicative (Duplex i) where
  pure = Duplex (const mempty) <<< pure

instance profunctorRequestDuplex :: Profunctor Duplex where
  dimap f g (Duplex enc dec) = Duplex (f >>> enc) (g <$> dec)

parse :: forall i o. Duplex i o -> ServerRequest -> Aff (Either ParsingError o)
parse (Duplex _ dec) req = Parser.run dec req

print :: forall i o. Duplex i o -> i -> ClientRequest
print (Duplex enc _) = Printer.run <<< enc

body
  :: forall i o
   . (i -> ClientBody)
  -> (ServerRequestBody -> Aff (Either String o))
  -> Duplex i o
body prtBody prsBody = do
  Duplex
    (Printer.body <<< prtBody)
    (Parser.body prsBody)

root :: forall i o. Duplex i o -> Duplex i o
root = path ""

path :: forall i o. String -> Duplex i o -> Duplex i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

-- | `show` is a function used to construct parsing error message.
as
  :: forall i i' o o'
   . { print :: i' -> i, parse :: o -> Either String o', show :: o -> String }
  -> Duplex i o
  -> Duplex i' o'
as { print: prt, parse: prs, show } (Duplex enc dec) = Duplex (enc <<< prt) (Parser.as { show, parse: prs } dec)

as' :: forall i i' o o'. Show o => (i' -> i) -> (o -> Either String o') -> Duplex i o -> Duplex i' o'
as' prt prs = as { print: prt, parse: prs, show: show }

int :: Duplex' String -> Duplex' Int
int = as' show Parser.int

json :: Duplex' String -> Duplex' Json
json = as' stringify jsonParser

prefix :: forall i o. String -> Duplex i o -> Duplex i o
prefix s (Duplex enc dec) = Duplex (Printer.prefix s <<< enc) (Parser.prefix s dec)

flag :: String -> Duplex Boolean Boolean
flag n = Duplex (Printer.flag n) (Parser.flag n)

string :: Duplex' String -> Duplex' String
string = as' identity pure

segment :: Duplex' String
segment = Duplex Printer.put Parser.take

param :: String -> Duplex' String
param p = Duplex (Printer.param p) (Parser.param p)

withMethod :: forall i o. Data.HTTP.Method -> Duplex i o -> Duplex i o
withMethod m (Duplex enc dec) = Duplex (append (Printer.method m) <<< enc) (Parser.method m dec)

rest :: Duplex' (Array String)
rest = Duplex (foldMap Printer.put) Parser.rest

params :: Duplex Params Params
params = Duplex (foldMap (uncurry Printer.param)) Parser.params

optional :: forall i o. Duplex i o -> Duplex (Maybe i) (Maybe o)
optional (Duplex enc dec) = Duplex (foldMap enc) (Parser.optional dec)
