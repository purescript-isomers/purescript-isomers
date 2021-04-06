module Isomers.Request.Accum.Type where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldr)
import Data.HTTP.Method (Method) as HTTP.Method
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.String (Pattern(..), split) as String
import Effect.Aff (Aff)
import Isomers.Contrib.Heterogeneous.List (type (:), HNil(..), (:))
import Isomers.Request.Duplex.Parser (ParsingError)
import Isomers.Request.Duplex.Parser (method, prefix, run) as Parser
import Isomers.Request.Duplex.Printer (method, prefix, run) as Printer
import Isomers.Request.Duplex.Type (Duplex(..), Duplex')
import Isomers.Request.Types (ClientRequest, ServerRequest)
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get, insert) as Record
import Type.Equality (from, to) as Type.Equality
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy)

data Accum body route ireq oreq
  = Accum (Duplex body ireq (route → oreq)) (ireq → route)

imapRoute ∷ ∀ body ireq oreq route route'. (route' → route) → (route → route') → Accum body route ireq oreq → Accum body route' ireq oreq
imapRoute f g (Accum dpl dst) = Accum (map (lcmap f) dpl) (map g dst)

unifyRoute ∷ ∀ body ireq oreq route route'. TypeEquals route route' ⇒ Accum body route ireq oreq → Accum body route' ireq oreq
unifyRoute = imapRoute Type.Equality.from Type.Equality.to

derive instance functorAccum ∷ Functor (Accum body route ireq)

instance applyAccum ∷ Apply (Accum body route ireq) where
  apply (Accum d1 dst) (Accum d2 _) = Accum (map apply d1 <*> d2) dst

instance profunctorAccum ∷ Profunctor (Accum body route) where
  dimap f g (Accum d dst) = Accum (dimap f (map g) d) (dst <<< f)

insert ∷
  ∀ a route route' body l ireq oreq.
  IsSymbol l ⇒
  Row.Cons l a route route' ⇒
  Row.Lacks l route ⇒
  SProxy l →
  Duplex' body a →
  Accum body { | route' } ireq oreq →
  Accum body { | route } ireq oreq
insert l (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      acc = dst ireq
    prt (Record.get l acc) <> dprt ireq

  dst' = Record.delete l <<< dst

  prs' = ado
    a ← prs
    f ← dprs
    in \acc →
      f (Record.insert l a acc)

hnil ∷ ∀ body ireq oreq. Accum body HNil ireq oreq → Accum body (Record ()) ireq oreq
hnil accum = scalar (pure HNil) accum

hcons ∷
  ∀ h body t ireq oreq.
  Duplex' body h →
  Accum body (h : t) ireq oreq →
  Accum body t ireq oreq
hcons (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      (h : t) = dst ireq
    prt h <> dprt ireq

  tail (h : t) = t

  dst' = tail <<< dst

  prs' = ado
    h ← prs
    f ← dprs
    in \t →
      f (h : t)

scalar ∷
  ∀ a body ireq oreq.
  Duplex' body a →
  Accum body a ireq oreq →
  Accum body {} ireq oreq
scalar (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      a = dst ireq
    prt a <> dprt ireq

  dst' = const {}

  prs' = ado
    a ← prs
    f ← dprs
    in const $ f a

prefix ∷ ∀ body route i o. String → Accum body route i o → Accum body route i o
prefix s (Accum (Duplex enc dec) dst) = do
  let
    dpl = Duplex (Printer.prefix s <$> enc) (Parser.prefix s dec)
  Accum dpl dst

path ∷ ∀ body route i o. String → Accum body route i o → Accum body route i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

root ∷ ∀ body route i o. Accum body route i o → Accum body route i o
root = path ""

parse ∷ ∀ body route i o. Accum body route i o → ServerRequest body → route → Aff (Either ParsingError o)
parse (Accum (Duplex _ dec) dst) req r = ado
  f ← Parser.run dec req
  in f <@> r

print ∷ ∀ body route i o. Accum body route i o → i → ClientRequest
print (Accum (Duplex enc _) _) = Printer.run <<< enc

method ∷ ∀ body route i o. HTTP.Method.Method → Accum body route i o → Accum body route i o
method m (Accum (Duplex enc dec) dst) = do
  let
    dpl = Duplex
      (append (Printer.method m) <$> enc)
      (Parser.method m dec)
  Accum dpl dst

recordDuplex ∷ ∀ body i o. Accum body {} i o → Duplex body i o
recordDuplex (Accum (Duplex prt prs) _) = Duplex (prt) (prs <@> {})

