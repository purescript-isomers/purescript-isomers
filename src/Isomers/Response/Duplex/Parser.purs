module Isomers.Response.Duplex.Parser where

import Prelude

import Control.Alt (class Alt)
import Control.Lazy (class Lazy) as Control
import Control.Monad.Error.Class (class MonadThrow, catchError)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (ask, asks) as Reader
import Control.Monad.State (StateT(..), evalStateT)
import Control.Monad.State (get, put) as State
import Data.Argonaut (Json)
import Data.Array (uncons) as Array
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String (Pattern(..), split) as String
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant (default, inj, on) as Variant
import Debug.Trace (traceM)
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error) as Effect.Exception
import Isomers.Contrib.Data.Variant (tag) as Contrib.Data.Variant
import Isomers.Response.Encodings (ClientHeaders, ClientResponse, ClientBodyRow) as Encodings
import Isomers.Response.Encodings (ClientResponse(..))
import Network.HTTP.Types (HeaderName, hContentType)
import Network.HTTP.Types (Status) as HTTP.Types
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol)

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
  (ExceptT ParsingError (StateT (Maybe (Variant Encodings.ClientBodyRow)) (ReaderT Encodings.ClientResponse Aff)) o)

derive instance newtypeParser ∷ Newtype (Parser o) _

derive newtype instance functorParser ∷ Functor (Parser)

derive newtype instance applyParser ∷ Apply (Parser)

derive newtype instance applicativeParser ∷ Applicative (Parser)

derive newtype instance bindParser ∷ Bind (Parser)

derive newtype instance monadParser ∷ Monad (Parser)

derive newtype instance monadParserThrow ∷ MonadThrow ParsingError (Parser)

-- newtype StateT s m a = StateT (s → m (Tuple a s))
instance lazyParser ∷ Control.Lazy (Parser a) where
  defer f = Parser $ ExceptT $ StateT \s → do
    let
      Parser (ExceptT (StateT f')) = f unit
    f' s

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
  Row.Cons l (Fiber a) br_ Encodings.ClientBodyRow ⇒
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
                    >>= \(ClientResponse { body: b }) → do
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

status ∷ Parser HTTP.Types.Status
status = Parser $ Reader.asks (_.status <<< un ClientResponse)

statusEquals ∷ HTTP.Types.Status → Parser Unit
statusEquals { code: expected } =
  status
    >>= \{ code: got } →
        when (expected /= got) do
          throwError $ Expected ("Status code: " <> show expected) (show got)

headers ∷ Parser Encodings.ClientHeaders
headers = Parser $ Reader.asks (_.headers <<< un ClientResponse)

header ∷ HeaderName → Parser (Maybe String)
header hn = headers <#> Map.lookup hn

reqHeader ∷ HeaderName → Parser String
reqHeader hn =
  header hn
    >>= case _ of
        Nothing → throwError (HeaderMissing hn)
        Just v → pure v

run ∷ ∀ a. Parser a → Encodings.ClientResponse → Aff (Either ParsingError a)
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

withContentType ∷ ∀ a. String → Parser a → Parser a
withContentType expected prs = do
  ( reqHeader hContentType >>= String.split (String.Pattern ";") >>> Array.uncons >>> case _ of
      Just { head: got } → do
        if (expected /= got)
          then fail got
          else prs
      Nothing → fail ""
  )
  where
    fail got = throwError (Expected (printHeaderName hContentType <> ":" <> expected) got)

printHeaderName ∷ CaseInsensitiveString → String
printHeaderName = un CaseInsensitiveString

withHeaderPassing ∷ ∀ a. HeaderName → (String → Boolean) → String → Parser a → Parser a
withHeaderPassing hn check err prs =
  reqHeader hn >>= \got → do
      if check got
        then throwError (Expected (printHeaderName hn <> ":" <> err) got)
        else prs

withHeaderValue ∷ ∀ a. CaseInsensitiveString → String → Parser a → Parser a
withHeaderValue hn expected prs = withHeaderPassing hn (eq expected) expected prs

