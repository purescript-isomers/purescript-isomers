module Isomers.Request.Duplex.Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foldable (lookup) as Foldable
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method) as HTTP
import Data.HTTP.Method (print) as HTTP.Method
import Data.Int as Int
import Data.Lazy (Lazy)
import Data.Lazy (force) as Lazy
import Data.Lazy as Z
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Data.Variant (default, inj, on) as Variant
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Class (liftEffect)
import Isomers.Contrib.Data.Variant (tag) as Contrib.Data.Variant
import Isomers.Request.Duplex.Path (Params)
import Isomers.Request.Duplex.Path (Parts, parse) as Path
import Isomers.Request.Encodings (ServerRequest)
import Network.HTTP.Types (HeaderName)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy, reflectSymbol)

-- | This quick and dirty `body` representation which was
-- | done to simplify generic spec processing is going to change.
-- | Please read some suggestions / details in `Isomers/Request/Encodings.purs`.
type State (body :: Row Type) =
  { body :: Either (Effect { | body }) (Maybe (Variant body))
  , headers :: Lazy (Map HeaderName String)
  , httpVersion :: String
  , method :: String
  , path :: Path.Parts
  }

data Result body a
  = Fail ParsingError
  | Success (State body) a

derive instance functorResult :: Functor (Result b)

derive instance genericResult :: Generic (Result b a) _

-- derive instance eqResult :: Eq a => Eq (Result b a)
-- instance showResult :: Show a => Show (Result b a) where
--   show = genericShow
data ParsingError
  = Expected String String
  | ExpectedEndOfPath String
  | ExpectedMethod String String
  | ExpectedHeaderValue HeaderName String
  | MissingHeader HeaderName
  | MissingParam String
  | EndOfPath

derive instance eqRouteError :: Eq ParsingError

derive instance genericRouteError :: Generic ParsingError _

instance showRouteError :: Show ParsingError where
  show = genericShow

-- | TODO: try to parametrize this by any monad
data Parser body a
  = Alt (NonEmptyArray (Parser body a))
  | Chomp (State body -> Aff (Result body a))
  | Prefix String (Parser body a)
  | Method String (Parser body a)

derive instance functorParser :: Functor (Parser body)

instance applyParser :: Apply (Parser body) where
  apply fx x =
    Chomp \state ->
      runParser state fx
        >>= case _ of
          Fail err -> pure $ Fail err
          Success state' f -> map f <$> runParser state' x

instance applicativeParser :: Applicative (Parser body) where
  pure a = Chomp $ pure <<< flip Success a

instance altParser :: Alt (Parser body) where
  alt (Alt ls) (Alt rs) = Alt (ls `altAppend` rs)
  alt (Alt ls) b = Alt (ls `altSnoc` b)
  alt a (Alt rs) = Alt (a `altCons` rs)
  alt (Prefix pre a) (Prefix pre' b)
    | pre == pre' = Prefix pre (a <|> b)
  alt (Method m a) (Method m' b)
    | m == m' = Method m (a <|> b)
  alt a b = Alt (NEA.cons a (NEA.singleton b))

instance lazyParser :: Lazy (Parser body a) where
  defer k =
    Chomp \state ->
      runParser state (Z.force parser)
    where
    parser = Z.defer k

altAppend
  :: forall a body
   . NonEmptyArray (Parser body a)
  -> NonEmptyArray (Parser body a)
  -> NonEmptyArray (Parser body a)
altAppend ls rs
  | Prefix pre a <- NEA.last ls
  , Prefix pre' b <- NEA.head rs
  , pre == pre' =
      let
        rs' = NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)
      in
        case NEA.fromArray (NEA.init ls) of
          Just ls' -> ls' `altAppend` rs'
          Nothing -> rs'
  | Method m a <- NEA.last ls
  , Method m' b <- NEA.head rs
  , m == m' =
      let
        rs' = NEA.cons' (Method m (a <|> b)) (NEA.tail rs)
      in
        case NEA.fromArray (NEA.init ls) of
          Just ls' -> ls' `altAppend` rs'
          Nothing -> rs'
  | otherwise = ls <> rs

altCons
  :: forall a body
   . Parser body a
  -> NonEmptyArray (Parser body a)
  -> NonEmptyArray (Parser body a)
altCons (Prefix pre a) rs
  | Prefix pre' b <- NEA.head rs
  , pre == pre' = NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)

altCons (Method m a) rs
  | Method m' b <- NEA.head rs
  , m == m' = NEA.cons' (Method m (a <|> b)) (NEA.tail rs)

altCons a rs = NEA.cons a rs

altSnoc
  :: forall a body
   . NonEmptyArray (Parser body a)
  -> Parser body a
  -> NonEmptyArray (Parser body a)
altSnoc ls (Prefix pre b)
  | Prefix pre' a <- NEA.last ls
  , pre == pre' = NEA.snoc' (NEA.init ls) (Prefix pre (a <|> b))

altSnoc ls (Method m b)
  | Method m' a <- NEA.last ls
  , m == m' = NEA.snoc' (NEA.init ls) (Method m (a <|> b))

altSnoc ls b = NEA.snoc ls b

chompPrefix :: forall body. String -> State body -> Result body Unit
chompPrefix pre state = case Array.head state.path.segments of
  Just pre'
    | pre == pre' -> Success (state { path { segments = Array.drop 1 state.path.segments } }) unit
  Just pre' -> Fail $ Expected pre pre'
  _ -> Fail $ EndOfPath

prefix :: forall a body. String -> Parser body a -> Parser body a
prefix = Prefix

method :: forall a body. HTTP.Method -> Parser body a -> Parser body a
method = Method <<< HTTP.Method.print <<< Left

body :: forall a body body_ l. IsSymbol l => Row.Cons l (Fiber a) body_ body => Proxy l -> Parser body a
body l =
  Chomp \state -> case state.body of
    Left lbr -> do
      br <- liftEffect lbr
      let
        fb = Record.get l br
      b <- joinFiber fb
      let
        vb = Variant.inj l fb

        state' = state { body = Right (Just vb) }
      pure $ Success state' b
    Right (Just b) ->
      Variant.on
        l
        (map (Success state) <<< joinFiber)
        (Variant.default (pure $ Fail $ Expected (reflectSymbol l) (Contrib.Data.Variant.tag b)))
        b
    Right Nothing -> pure $ Fail $ Expected (reflectSymbol l) ("Empty body")

-- | TODO: We should be able to constraint the value of the body
-- | to empty when we provide some common layer for body parsing
-- | which can be considered portable like `String` ;-)
-- | Then we can provide this helper.
-- emptyBody = ...
take :: forall m. Parser m String
take =
  Chomp \state ->
    pure
      $ case Array.uncons state.path.segments of
          Just { head, tail } -> Success (state { path { segments = tail } }) head
          _ -> Fail EndOfPath

param :: forall m. String -> Parser m String
param key =
  Chomp \state ->
    pure
      $ case Foldable.lookup key state.path.params of
          Just a -> Success state a
          _ -> Fail $ MissingParam key

header :: forall body. HeaderName -> Parser body String
header name =
  Chomp \state ->
    pure
      $ case Map.lookup name $ Lazy.force state.headers of
          Just a -> Success state a
          _ -> Fail $ MissingHeader name

flag :: forall m. String -> Parser m Boolean
flag = default false <<< map (const true) <<< param

many1 :: forall body t a. Alt t => Applicative t => Parser body a -> Parser body (t a)
many1 p = Chomp go
  where
  go :: State body -> Aff (Result body (t a))
  go state =
    runParser state p
      >>= case _ of
        Fail err -> pure $ Fail err
        Success state' a -> go' state' (pure a)

  go' :: State body -> t a -> Aff (Result body (t a))
  go' state xs =
    runParser state p
      >>= case _ of
        Fail _ -> pure $ Success state xs
        Success state' a -> go' state' (xs <|> pure a)

many :: forall t a body. Alternative t => Parser body a -> Parser body (t a)
many p = many1 p <|> pure empty

rest :: forall b. Parser b (Array String)
rest = Chomp \state -> pure $ Success (state { path { segments = [] } }) state.path.segments

params :: forall b. Parser b Params
params = Chomp \state -> pure $ Success state state.path.params

default :: forall a body. a -> Parser body a -> Parser body a
default = flip (<|>) <<< pure

optional :: forall a body. Parser body a -> Parser body (Maybe a)
optional = default Nothing <<< map Just

as :: forall a b body. { show :: a -> String, parse :: a -> Either String b } -> Parser body a -> Parser body b
as { parse, show } p =
  Chomp \state ->
    runParser state p
      >>= \r ->
        pure
          $ case r of
              Fail err -> Fail err
              Success state' a -> case parse a of
                Left err -> Fail $ Expected err (show a)
                Right b -> Success state' b

int :: String -> Either String Int
int = maybe (Left "Int") Right <<< Int.fromString

hash :: forall body. Parser body String
hash = Chomp \state -> pure $ Success state state.path.hash

end :: forall body. Parser body Unit
end =
  Chomp \state ->
    pure
      $ case Array.head state.path.segments of
          Nothing -> Success state unit
          Just str -> Fail (ExpectedEndOfPath str)

boolean :: String -> Either String Boolean
boolean = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left "Boolean"

runParser :: forall a body. State body -> Parser body a -> Aff (Result body a)
runParser = go
  where
  go state = case _ of
    Alt xs -> foldl (\acc p -> acc >>= goAlt state p) (pure $ Fail EndOfPath) xs
    Chomp f -> f state
    Method m p ->
      if state.method /= m then
        pure $ Fail $ ExpectedMethod m state.method
      else
        go state p
    Prefix pre p -> case chompPrefix pre state of
      Fail err -> pure $ Fail err
      Success state' _ -> go state' p

  goAlt state p (Fail _) = runParser state p

  goAlt _ _ res = pure res

run :: forall a body. Parser body a -> ServerRequest body -> Aff (Either ParsingError a)
run p = do
  parsePath' >>> flip runParser p
    <#> map case _ of
      Fail err -> Left err
      Success _ res -> Right res
  where
  parsePath' { body: b, headers, httpVersion, method: m, path } = { body: b, headers, httpVersion, method: m, path: _ }
    $ Path.parse path
