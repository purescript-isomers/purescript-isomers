module Isomers.Request.Accum.Type where

import Prelude

import Data.Either (Either)
import Data.Foldable (foldr)
import Data.HTTP.Method (Method) as HTTP.Method
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.String (Pattern(..), split) as String
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Isomers.Contrib.Heterogeneous.List (type (:), HNil(..), (:))
import Isomers.Request.Duplex.Parser (ParsingError)
import Isomers.Request.Duplex.Parser (method, prefix, run) as Parser
import Isomers.Request.Duplex.Printer (method, prefix, run) as Printer
import Isomers.Request.Duplex.Type (Duplex(..), Duplex')
import Isomers.Request.Encodings (ClientRequest, ServerRequest)
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, get, insert) as Record
import Type.Equality (from, to) as Type.Equality
import Type.Prelude (class IsSymbol, class TypeEquals, Proxy)

-- | The Problem
-- |
-- |  * Let's imagine that we have a `Duplex` which transforms a
-- |    path segment into / from `Integer`.
-- |
-- |  * Let's imagine that we have a `Duplex` which transforms next
-- |    path segment into a `Variant` which should be filled with previously
-- |    parsed value.
-- |    ```
-- |    { id: Integer } ->
-- |      Variant ("profile" :: { id: Integer }, "edit-profile" :: { id: Integer })
-- |    ```
-- |
-- |  * We want to provide DSL which allows user to express this flow in natural
-- |    manner - from left to right.
-- |
data Accum route ireq oreq = Accum (Duplex ireq (route -> oreq)) (ireq -> route)

derive instance functorAccum :: Functor (Accum route ireq)

instance applyAccum :: Apply (Accum route ireq) where
  apply (Accum d1 dst) (Accum d2 _) = Accum (map apply d1 <*> d2) dst

instance profunctorAccum :: Profunctor (Accum route) where
  dimap f g (Accum d dst) = Accum (dimap f (map g) d) (dst <<< f)

pass :: forall route. Accum route route route
pass = Accum (pure identity) identity

-- | When we have same request type we can provide a categorical
-- | composition and nicely chain accumulation of our final value
-- | as well as its printer a la `Boomerang` it boils down to
-- | just composition over pairs of functions:
-- |
-- | ```
-- |    (b -> c) /\ (c -> b)
-- | -> (a -> b) /\ (b -> a)
-- | -> (a -> c) /\ (c -> a)
-- | ```
newtype RouteAccum route req = RouteAccum (Accum route req req)

instance semigroupoidRouteAccum :: Semigroupoid (RouteAccum) where
  compose (RouteAccum (Accum (Duplex ser1 prs1) ext1)) (RouteAccum (Accum (Duplex ser2 prs2) ext2)) = RouteAccum $ Accum
    dpl
    ext
    where
    prs = ado
      g <- prs2
      f <- prs1
      in f <<< g

    ser i = ser1 i <> ser2 (ext1 i)

    dpl = Duplex ser prs

    ext = ext2 <<< ext1

instance categoryRouteAccum :: Category (RouteAccum) where
  identity = RouteAccum (Accum (pure identity) identity)

insert
  :: forall a route route' l ireq oreq
   . IsSymbol l
  => Row.Cons l a route route'
  => Row.Lacks l route
  => Proxy l
  -> Duplex' a
  -> Accum { | route' } ireq oreq
  -> Accum { | route } ireq oreq
insert l (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = prt (Record.get l $ dst ireq) <> dprt ireq

  dst' = Record.delete l <<< dst

  prs' = ado
    a <- prs
    f <- dprs
    in
      \acc ->
        f (Record.insert l a acc)

-- | Like `insert` but actual "`a` parsing / printing"
-- | is put on the front.
insertFlipped
  :: forall a route route' l ireq oreq
   . IsSymbol l
  => Row.Cons l a route route'
  => Row.Lacks l route
  => Proxy l
  -> Duplex' a
  -> Accum { | route' } ireq oreq
  -> Accum { | route } ireq oreq
insertFlipped l (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = dprt ireq <> prt (Record.get l $ dst ireq)

  dst' = Record.delete l <<< dst

  prs' = ado
    f <- dprs
    a <- prs
    in
      \acc ->
        f (Record.insert l a acc)

-- | This insert is useful when we do
-- | "finall" insert of the "body" etc.
-- | because we want to be able to "split"
-- | server / client request types.
insertReq
  :: forall a b route l ireq oreq
   . IsSymbol l
  => Row.Cons l a route ireq
  => Row.Cons l b route oreq
  => Row.Lacks l route
  => Proxy l
  -> Duplex a b
  -> Accum { | route } { | ireq } { | oreq }
insertReq l (Duplex prt prs) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = prt (Record.get l ireq)

  dst' = Record.delete l

  prs' = ado
    a <- prs
    in \acc -> Record.insert l a acc

-- | This can be useful when you want to move into `Tuple` or `HList` accumlation or
-- | just have to lift a single request `Duplex` into an `Accum`.
scalar
  :: forall a ireq oreq
   . Duplex' a
  -> Accum a ireq oreq
  -> Accum {} ireq oreq
scalar (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') (const {})
  where
  prt' ireq = prt (dst ireq) <> dprt ireq

  prs' = ado
    a <- prs
    f <- dprs
    in const $ f a

scalarFlipped
  :: forall a ireq oreq
   . Duplex' a
  -> Accum a ireq oreq
  -> Accum {} ireq oreq
scalarFlipped (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') (const {})
  where
  prt' ireq = dprt ireq <> prt (dst ireq)

  prs' = ado
    f <- dprs
    a <- prs
    in const $ f a

-- | Switch from `Record` accumlator into `HList` one.
hnil :: forall ireq oreq. Accum HNil ireq oreq -> Accum {} ireq oreq
hnil accum = scalar (pure HNil) accum

hcons
  :: forall h t ireq oreq
   . Duplex' h
  -> Accum (h : t) ireq oreq
  -> Accum t ireq oreq
hcons (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      (h : _) = dst ireq
    prt h <> dprt ireq

  tail (_ : t) = t

  dst' = tail <<< dst

  prs' = ado
    h <- prs
    f <- dprs
    in
      \t ->
        f (h : t)

tupleCons
  :: forall a b ireq oreq
   . Duplex' a
  -> Accum (a /\ b) ireq oreq
  -> Accum b ireq oreq
tupleCons (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      (a /\ _) = dst ireq
    prt a <> dprt ireq

  dst' = snd <<< dst

  prs' = ado
    a <- prs
    f <- dprs
    in
      \b ->
        f (a /\ b)

prefix :: forall route i o. String -> Accum route i o -> Accum route i o
prefix s (Accum (Duplex enc dec) dst) = do
  let
    dpl = Duplex (Printer.prefix s <$> enc) (Parser.prefix s dec)
  Accum dpl dst

path :: forall route i o. String -> Accum route i o -> Accum route i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

parse :: forall route i o. Accum route i o -> ServerRequest -> route -> Aff (Either ParsingError o)
parse (Accum (Duplex _ dec) _) req r = ado
  f <- Parser.run dec req
  in f <@> r

print :: forall route i o. Accum route i o -> i -> ClientRequest
print (Accum (Duplex enc _) _) = Printer.run <<< enc

method :: forall route i o. HTTP.Method.Method -> Accum route i o -> Accum route i o
method m (Accum (Duplex enc dec) dst) = do
  let
    dpl =
      Duplex
        (append (Printer.method m) <$> enc)
        (Parser.method m dec)
  Accum dpl dst

imapRoute
  :: forall ireq oreq route route'
   . (route' -> route)
  -> (route -> route')
  -> Accum route ireq oreq
  -> Accum route' ireq oreq
imapRoute f g (Accum dpl dst) = Accum (map (lcmap f) dpl) (map g dst)

unifyRoute
  :: forall ireq oreq route route'
   . TypeEquals route route'
  => Accum route ireq oreq
  -> Accum route' ireq oreq
unifyRoute = imapRoute Type.Equality.from Type.Equality.to

rootDuplex :: forall i o. Accum {} i o -> Duplex i o
rootDuplex accum = do
  let
    (Accum (Duplex prt prs) _) = path "" accum
  Duplex (prt) (prs <@> {})
