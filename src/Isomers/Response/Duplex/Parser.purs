module Isomers.Response.Duplex.Parser where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow, catchError)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (ask, asks) as Reader
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State (get, put) as State
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant (default, inj, on) as Variant
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error) as Effect.Exception
import Isomers.Contrib.Data.Variant (tag) as Contrib.Data.Variant
import Isomers.HTTP.Response (Web, WebBodyRow, WebHeaders) as HTTP.Response
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types (Status) as HTTP.Types
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol)
import Web.Streams.ReadableStream (ReadableStream) as Web.Streams

-- | TODO: Error handling
-- | * Use `V` here.
-- | * Provide proper error type
data ParsingError
  = Expected String String
  | BodyParsingError String
  | HeaderMissing HeaderName
  | JSError Effect.Exception.Error

-- | We want to be able to alter between body types during parsing
-- | so it is easier to avoid type level indexing here I think.
newtype Parser o
  = Parser
  (ExceptT ParsingError (StateT (Maybe (Variant HTTP.Response.WebBodyRow)) (ReaderT HTTP.Response.Web Aff)) o)

derive instance newtypeParser ∷ Newtype (Parser o) _

derive newtype instance functorParser ∷ Functor (Parser)

derive newtype instance applyParser ∷ Apply (Parser)

derive newtype instance applicativeParser ∷ Applicative (Parser)

derive newtype instance bindParser ∷ Bind (Parser)

derive newtype instance monadParser ∷ Monad (Parser)

derive newtype instance monadParserThrow ∷ MonadThrow ParsingError (Parser)

_responseParserError = SProxy ∷ SProxy "responseParserError"

instance altParser ∷ Alt (Parser) where
  alt (Parser (ExceptT p1)) (Parser (ExceptT p2)) =
    Parser $ ExceptT
      $ do
          p1
            >>= case _ of
                r@(Right o) → pure r
                otherwise → p2

readBody ∷
  ∀ a br_ l.
  Row.Cons l (Fiber a) br_ HTTP.Response.WebBodyRow ⇒
  IsSymbol l ⇒
  SProxy l → Parser a
readBody l =
  Parser
    $ do
        fb ←
          State.get
            >>= case _ of
                Just fb → do
                  let
                    Parser err =
                      throwError
                        $ Expected (reflectSymbol l)
                        $ (Contrib.Data.Variant.tag fb)
                  Variant.on l pure (Variant.default err) fb
                Nothing →
                  Reader.ask
                    >>= \{ body: b } → do
                        let
                          fb = Record.get l b
                        State.put (Just (Variant.inj l fb))
                        pure fb
        liftAff $ joinFiber fb

arrayBuffer ∷ Parser ArrayBuffer
arrayBuffer = readBody (SProxy ∷ SProxy "arrayBuffer")

json ∷ Parser Json
json = readBody (SProxy ∷ SProxy "json")

string ∷ Parser String
string = readBody (SProxy ∷ SProxy "string")

stream ∷ Parser (Web.Streams.ReadableStream Uint8Array)
stream = readBody (SProxy ∷ SProxy "stream")

status ∷ Parser HTTP.Types.Status
status = Parser $ Reader.asks _.status

statusEquals ∷ HTTP.Types.Status → Parser Unit
statusEquals { code: expected } =
  status
    >>= \{ code: got } →
        when (expected /= got) do
          throwError $ Expected ("Status code: " <> show expected) (show got)

headers ∷ Parser HTTP.Response.WebHeaders
headers = Parser $ Reader.asks _.headers

header ∷ HeaderName → Parser (Maybe String)
header hn = headers <#> Map.lookup hn

reqHeader ∷ HeaderName → Parser String
reqHeader hn =
  header hn
    >>= case _ of
        Nothing → throwError (HeaderMissing hn)
        Just v → pure v

run ∷ ∀ a. Parser a → HTTP.Response.Web → Aff (Either ParsingError a)
run (Parser prs) i = go `catchError` (JSError >>> Left >>> pure)
  where
  go = flip runReaderT i <<< flip evalStateT Nothing <<< runExceptT $ prs

fromString ∷ ∀ a b. (a → String) → (a → Either String b) → Parser a → Parser b
fromString print decode p = do
  a ← p
  case decode a of
    Left err → throwError $ Expected err (print a)
    Right b → pure b

fromJson ∷ ∀ b. (Json → Either String b) → Parser b
fromJson decode = do
  json >>= \j → case decode j  of
    Left err → throwError $ BodyParsingError err
    Right b → pure b


