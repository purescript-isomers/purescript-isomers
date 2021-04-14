module Isomers.Request.Duplex.Printer where

import Prelude

import Data.Array as Array
import Data.HTTP.Method (Method(..)) as HTTP.Method
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\))
import Global.Unsafe (unsafeEncodeURIComponent)
import Isomers.Request.Duplex.Path (Parts) as Path
import Isomers.Request.Encodings (ClientBody, ClientRequest)
import Network.HTTP.Types (HeaderName)

type State =
  { body ∷ Maybe ClientBody
  , headers ∷ Array (HeaderName /\ String)
  , method ∷ HTTP.Method.Method
  , path :: Path.Parts
  }

emptyUrlParts :: Path.Parts
emptyUrlParts =
  { segments: []
  , params: []
  , hash: ""
  }

emptyRequestState :: State
emptyRequestState =
  { body: Nothing
  , headers: []
  , method: HTTP.Method.GET
  , path: emptyUrlParts
  }

newtype Printer
  = Printer (State -> State)

derive instance newtypePrinter :: Newtype Printer _

instance semigroupPrinter :: Semigroup Printer where
  append (Printer f) (Printer g) = Printer (f >>> g)

instance monoidRoutePRinter :: Monoid Printer where
  mempty = Printer identity

put :: String -> Printer
put str = Printer \state -> state { path { segments = Array.snoc state.path.segments str } }

param :: String -> String -> Printer
param key val = Printer \state -> state { path { params = Array.cons (Tuple key val) state.path.params } }

header :: HeaderName -> String -> Printer
header name val = Printer \state -> state { headers = Array.cons (Tuple name val) state.headers }

flag :: String -> Boolean -> Printer
flag key val
  | val = param key ""
  | otherwise = mempty

hash :: String -> Printer
hash h = Printer _ { path { hash = h } }

method :: HTTP.Method.Method -> Printer
method str = Printer \state -> state { method = str }

printPath :: Path.Parts -> String
printPath { segments, params, hash: hash' } = printSegments segments <> printParams params <> printHash hash'
  where
  printSegments = case _ of
    [ "" ] -> "/"
    xs -> joinWith "/" $ map unsafeEncodeURIComponent xs

  printParams [] = ""

  printParams ps = "?" <> joinWith "&" (uncurry printParam <$> ps)

  printParam key "" = unsafeEncodeURIComponent key

  printParam key val = unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent val

  printHash "" = ""

  printHash h = "#" <> h

prefix :: String -> Printer -> Printer
prefix s prt = put s <> prt

body ∷ ClientBody → Printer
body b = Printer _{ body = Just b }

run ∷ Printer → ClientRequest
run (Printer enc) = do
  enc emptyRequestState # \st → st { path = printPath st.path }

