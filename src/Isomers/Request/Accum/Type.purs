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

-- | TODO: Would there be any gain when we have something like
-- | "builder yoneda" here instead of a plain `route → oreq` function:
-- |
-- | ```newtype BuilderYoneda bld i a = BuilderYoneda (∀ b. bld a b → bld i b)```
-- |
-- | with some form of a "functor":
-- |
-- | ```bldMap bA2B (BuilderYoneda f) = BuilderYoneda $ \bld → f (bld <<< bA2B)```
-- |
-- |
-- | In the case of a record build up there is a quite significant difference between
-- | possible performance of these two on the JS backend:
-- |
-- | * `Record.insert` - which is `unsafeSet` with a loop etc. under the hood:
-- | https://github.com/purescript/purescript-prelude/blob/v5.0.0/src/Record/Unsafe.js#L15
-- |
-- | * `Record.Builder.insert` - which is `unsafeInsert` underneath - nearly a plain record setter:
-- | https://github.com/purescript/purescript-record/blob/v3.0.0/src/Record/Builder.js#L13
-- |
-- | Another scenario for benchmarking could be this kind of crazy codegen based
-- | optimization of insert through js eval:
-- | ```
-- | insert l = eval """function(a, r) { return { x: r.x , y: r.y,.., l': a } """
-- | ```
-- | then cache the insert on the prs' level:
-- | ```
-- | prs' = let insert' = insert l in....
-- | ```
data Accum body route ireq oreq = Accum (Duplex body ireq (route -> oreq)) (ireq -> route)

derive instance functorAccum :: Functor (Accum body route ireq)

instance applyAccum :: Apply (Accum body route ireq) where
  apply (Accum d1 dst) (Accum d2 _) = Accum (map apply d1 <*> d2) dst

instance profunctorAccum :: Profunctor (Accum body route) where
  dimap f g (Accum d dst) = Accum (dimap f (map g) d) (dst <<< f)

-- | When we have same request type we can provide a categorical
-- | composition and nicely chain accumulation of our final value
-- | as well as their Printer.
newtype RouteAccum body route req = RouteAccum (Accum body route req req)

instance semigroupoidRouteAccum :: Semigroupoid (RouteAccum body) where
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

instance categoryRouteAccum :: Category (RouteAccum body) where
  identity = RouteAccum (Accum (pure identity) identity)

insert
  :: forall a route route' body l ireq oreq
   . IsSymbol l
  => Row.Cons l a route route'
  => Row.Lacks l route
  => Proxy l
  -> Duplex' body a
  -> Accum body { | route' } ireq oreq
  -> Accum body { | route } ireq oreq
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
  :: forall a route route' body l ireq oreq
   . IsSymbol l
  => Row.Cons l a route route'
  => Row.Lacks l route
  => Proxy l
  -> Duplex' body a
  -> Accum body { | route' } ireq oreq
  -> Accum body { | route } ireq oreq
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
  :: forall a b route body l ireq oreq
   . IsSymbol l
  => Row.Cons l a route ireq
  => Row.Cons l b route oreq
  => Row.Lacks l route
  => Proxy l
  -> Duplex body a b
  -> Accum body { | route } { | ireq } { | oreq }
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
  :: forall a body ireq oreq
   . Duplex' body a
  -> Accum body a ireq oreq
  -> Accum body {} ireq oreq
scalar (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') (const {})
  where
  prt' ireq = prt (dst ireq) <> dprt ireq

  prs' = ado
    a <- prs
    f <- dprs
    in const $ f a

scalarFlipped
  :: forall a body ireq oreq
   . Duplex' body a
  -> Accum body a ireq oreq
  -> Accum body {} ireq oreq
scalarFlipped (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') (const {})
  where
  prt' ireq = dprt ireq <> prt (dst ireq)

  prs' = ado
    f <- dprs
    a <- prs
    in const $ f a

-- | Switch from `Record` accumlator into `HList` one.
hnil :: forall body ireq oreq. Accum body HNil ireq oreq -> Accum body {} ireq oreq
hnil accum = scalar (pure HNil) accum

hcons
  :: forall h body t ireq oreq
   . Duplex' body h
  -> Accum body (h : t) ireq oreq
  -> Accum body t ireq oreq
hcons (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      (h : t) = dst ireq
    prt h <> dprt ireq

  tail (h : t) = t

  dst' = tail <<< dst

  prs' = ado
    h <- prs
    f <- dprs
    in
      \t ->
        f (h : t)

tupleCons
  :: forall a b body ireq oreq
   . Duplex' body a
  -> Accum body (a /\ b) ireq oreq
  -> Accum body b ireq oreq
tupleCons (Duplex prt prs) (Accum (Duplex dprt dprs) dst) = Accum (Duplex prt' prs') dst'
  where
  prt' ireq = do
    let
      (a /\ b) = dst ireq
    prt a <> dprt ireq

  dst' = snd <<< dst

  prs' = ado
    a <- prs
    f <- dprs
    in
      \b ->
        f (a /\ b)

prefix :: forall body route i o. String -> Accum body route i o -> Accum body route i o
prefix s (Accum (Duplex enc dec) dst) = do
  let
    dpl = Duplex (Printer.prefix s <$> enc) (Parser.prefix s dec)
  Accum dpl dst

path :: forall body route i o. String -> Accum body route i o -> Accum body route i o
path = flip (foldr prefix) <<< String.split (String.Pattern "/")

parse :: forall body route i o. Accum body route i o -> ServerRequest body -> route -> Aff (Either ParsingError o)
parse (Accum (Duplex _ dec) dst) req r = ado
  f <- Parser.run dec req
  in f <@> r

print :: forall body route i o. Accum body route i o -> i -> ClientRequest
print (Accum (Duplex enc _) _) = Printer.run <<< enc

method :: forall body route i o. HTTP.Method.Method -> Accum body route i o -> Accum body route i o
method m (Accum (Duplex enc dec) dst) = do
  let
    dpl =
      Duplex
        (append (Printer.method m) <$> enc)
        (Parser.method m dec)
  Accum dpl dst

imapRoute
  :: forall body ireq oreq route route'
   . (route' -> route)
  -> (route -> route')
  -> Accum body route ireq oreq
  -> Accum body route' ireq oreq
imapRoute f g (Accum dpl dst) = Accum (map (lcmap f) dpl) (map g dst)

unifyRoute
  :: forall body ireq oreq route route'
   . TypeEquals route route'
  => Accum body route ireq oreq
  -> Accum body route' ireq oreq
unifyRoute = imapRoute Type.Equality.from Type.Equality.to

rootDuplex :: forall body i o. Accum body {} i o -> Duplex body i o
rootDuplex accum = do
  let
    (Accum (Duplex prt prs) _) = path "" accum
  Duplex (prt) (prs <@> {})
