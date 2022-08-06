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
import Effect.Aff (Aff)
import Isomers.Request.Duplex.Path (Params)
import Isomers.Request.Duplex.Path (Parts, parse) as Path
import Isomers.Request.Encodings (ServerRequest, ServerRequestBody)
import Network.HTTP.Types (HeaderName)

-- | This quick and dirty `body` representation which was
-- | done to simplify generic spec processing is going to change.
-- | Please read some suggestions / details in `Isomers/Request/Encodings.purs`.
type State =
  { body :: Maybe ServerRequestBody
  , headers :: Lazy (Map HeaderName String)
  , httpVersion :: String
  , method :: String
  , path :: Path.Parts
  }

data Result a
  = Fail ParsingError
  | Success (State) a

derive instance functorResult :: Functor Result

derive instance genericResult :: Generic (Result a) _

-- derive instance eqResult :: Eq a => Eq (Result b a)
-- instance showResult :: Show a => Show (Result b a) where
--   show = genericShow
data ParsingError
  = BodyAlreadyConsumed
  | BodyParsingError String
  | Expected String String
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
data Parser a
  = Alt (NonEmptyArray (Parser a))
  | Chomp (State -> Aff (Result a))
  | Prefix String (Parser a)
  | Method String (Parser a)

derive instance functorParser :: Functor Parser

instance applyParser :: Apply Parser where
  apply fx x =
    Chomp \state ->
      runParser state fx
        >>= case _ of
          Fail err -> pure $ Fail err
          Success state' f -> map f <$> runParser state' x

instance applicativeParser :: Applicative Parser where
  pure a = Chomp $ pure <<< flip Success a

instance altParser :: Alt Parser where
  alt (Alt ls) (Alt rs) = Alt (ls `altAppend` rs)
  alt (Alt ls) b = Alt (ls `altSnoc` b)
  alt a (Alt rs) = Alt (a `altCons` rs)
  alt (Prefix pre a) (Prefix pre' b)
    | pre == pre' = Prefix pre (a <|> b)
  alt (Method m a) (Method m' b)
    | m == m' = Method m (a <|> b)
  alt a b = Alt (NEA.cons a (NEA.singleton b))

instance lazyParser :: Lazy (Parser a) where
  defer k =
    Chomp \state ->
      runParser state (Z.force parser)
    where
    parser = Z.defer k

altAppend
  :: forall a
   . NonEmptyArray (Parser a)
  -> NonEmptyArray (Parser a)
  -> NonEmptyArray (Parser a)
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
  :: forall a
   . Parser a
  -> NonEmptyArray (Parser a)
  -> NonEmptyArray (Parser a)
altCons (Prefix pre a) rs
  | Prefix pre' b <- NEA.head rs
  , pre == pre' = NEA.cons' (Prefix pre (a <|> b)) (NEA.tail rs)

altCons (Method m a) rs
  | Method m' b <- NEA.head rs
  , m == m' = NEA.cons' (Method m (a <|> b)) (NEA.tail rs)

altCons a rs = NEA.cons a rs

altSnoc
  :: forall a
   . NonEmptyArray (Parser a)
  -> Parser a
  -> NonEmptyArray (Parser a)
altSnoc ls (Prefix pre b)
  | Prefix pre' a <- NEA.last ls
  , pre == pre' = NEA.snoc' (NEA.init ls) (Prefix pre (a <|> b))

altSnoc ls (Method m b)
  | Method m' a <- NEA.last ls
  , m == m' = NEA.snoc' (NEA.init ls) (Method m (a <|> b))

altSnoc ls b = NEA.snoc ls b

chompPrefix :: String -> State -> Result Unit
chompPrefix pre state = case Array.head state.path.segments of
  Just pre'
    | pre == pre' -> Success (state { path { segments = Array.drop 1 state.path.segments } }) unit
  Just pre' -> Fail $ Expected pre pre'
  _ -> Fail $ EndOfPath

prefix :: forall a. String -> Parser a -> Parser a
prefix = Prefix

method :: forall a. HTTP.Method -> Parser a -> Parser a
method = Method <<< HTTP.Method.print <<< Left

body :: forall a. (ServerRequestBody -> Aff (Either String a)) -> Parser a
body f =
  Chomp \state -> case state.body of
    Just b -> do
      let state' = state { body = Nothing }
      f b >>= case _ of
        Right a -> pure $ Success state' a
        Left err -> pure $ Fail $ BodyParsingError err
    Nothing ->
      pure $ Fail $ BodyAlreadyConsumed

-- | TODO: We should be able to constraint the value of the
-- | to empty when we provide some common layer for parsing
-- | which can be considered portable like `String` ;-)
-- | Then we can provide this helper.
-- emptyBody = ...
take :: Parser String
take =
  Chomp \state ->
    pure
      $ case Array.uncons state.path.segments of
          Just { head, tail } -> Success (state { path { segments = tail } }) head
          _ -> Fail EndOfPath

param :: String -> Parser String
param key =
  Chomp \state ->
    pure
      $ case Foldable.lookup key state.path.params of
          Just a -> Success state a
          _ -> Fail $ MissingParam key

header :: HeaderName -> Parser String
header name =
  Chomp \state ->
    pure
      $ case Map.lookup name $ Lazy.force state.headers of
          Just a -> Success state a
          _ -> Fail $ MissingHeader name

flag :: String -> Parser Boolean
flag = default false <<< map (const true) <<< param

many1 :: forall t a. Alt t => Applicative t => Parser a -> Parser (t a)
many1 p = Chomp go
  where
  go :: State -> Aff (Result (t a))
  go state =
    runParser state p
      >>= case _ of
        Fail err -> pure $ Fail err
        Success state' a -> go' state' (pure a)

  go' :: State -> t a -> Aff (Result (t a))
  go' state xs =
    runParser state p
      >>= case _ of
        Fail _ -> pure $ Success state xs
        Success state' a -> go' state' (xs <|> pure a)

many :: forall t a. Alternative t => Parser a -> Parser (t a)
many p = many1 p <|> pure empty

rest :: Parser (Array String)
rest = Chomp \state -> pure $ Success (state { path { segments = [] } }) state.path.segments

params :: Parser Params
params = Chomp \state -> pure $ Success state state.path.params

default :: forall a. a -> Parser a -> Parser a
default = flip (<|>) <<< pure

optional :: forall a. Parser a -> Parser (Maybe a)
optional = default Nothing <<< map Just

as :: forall a b. { show :: a -> String, parse :: a -> Either String b } -> Parser a -> Parser b
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

hash :: Parser String
hash = Chomp \state -> pure $ Success state state.path.hash

end :: Parser Unit
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

runParser :: forall a. State -> Parser a -> Aff (Result a)
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

run :: forall a. Parser a -> ServerRequest -> Aff (Either ParsingError a)
run p = do
  parsePath' >>> flip runParser p
    <#> map case _ of
      Fail err -> Left err
      Success _ res -> Right res
  where
  parsePath' {body: b, headers, httpVersion, method: m, path } = {body: b, headers, httpVersion, method: m, path: _ }
    $ Path.parse path
