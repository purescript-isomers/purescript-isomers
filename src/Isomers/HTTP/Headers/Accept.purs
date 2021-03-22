module Isomers.HTTP.Headers.Accept where

import Prelude
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), split) as String

newtype MediaTypeType
  = MediaTypeType String

derive instance eqMediaTypeType ∷ Eq MediaTypeType

newtype MediaTypeSubtype
  = MediaTypeSubtype String

derive instance eqMediaTypeSubtype ∷ Eq MediaTypeSubtype

-- | Falling back to full `s`... it should never happen :-P
mediaTypeType ∷ MediaType → MediaTypeType
mediaTypeType (MediaType s) = case String.split (String.Pattern "/") s of
  [ mt, st ] → MediaTypeType mt
  otherwise → MediaTypeType s

mediaTypeSubtype ∷ MediaType → MediaTypeSubtype
mediaTypeSubtype (MediaType s) = case String.split (String.Pattern "/") s of
  [ mt, st ] → MediaTypeSubtype st
  otherwise → MediaTypeSubtype s

data MediaPattern
  = ProperMediaType MediaType
  -- | image/*, text/* etc.
  | AnySubtype MediaTypeType
  -- | */*
  | AnyMedia

-- | UnkonwnMediaPattern String
derive instance eqMediaPattern ∷ Eq MediaPattern

printMediaPattern ∷ MediaPattern → String
printMediaPattern = case _ of
  AnyMedia → "*/*"
  AnySubtype (MediaTypeType t) → t <> "/*"
  ProperMediaType (MediaType t) → t

parseMediaPattern ∷ String → MediaPattern
parseMediaPattern p
  | p == "*/*" = AnyMedia
  | [ mt, "*" ] ← String.split (String.Pattern "/") p = AnySubtype (MediaTypeType mt)
  | otherwise = ProperMediaType (MediaType p)

parseHeader ∷ String → Array { pattern ∷ MediaPattern, q ∷ Maybe String }
parseHeader h = do
  v ← String.split (String.Pattern ",") h
  case String.split (String.Pattern ";") v of
    [ m ] → [ { pattern: parseMediaPattern m, q: Nothing } ]
    [ m, q ] → [ { pattern: parseMediaPattern m, q: Just q } ]
    otherwise → []

matches ∷ MediaType → MediaPattern → Boolean
matches mt = do
  let
    -- Another micro optimization - we precomupute
    -- type from the media type...
    mtt = mediaTypeType mt
  case _ of
    ProperMediaType pt → pt == mt
    AnySubtype mtt' → mtt == mtt'
    AnyMedia → true
