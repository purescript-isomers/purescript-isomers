module Isomers.HTTP.Request.Headers.Accept.MediaPattern where

import Prelude
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

print ∷ MediaPattern → String
print = case _ of
  AnyMedia → "*/*"
  AnySubtype (MediaTypeType t) → t <> "/*"
  ProperMediaType (MediaType t) → t

parse ∷ String → MediaPattern
parse p
  | p == "*/*" = AnyMedia
  | [ mt, "*" ] ← String.split (String.Pattern "/") p = AnySubtype (MediaTypeType mt)
  | otherwise = ProperMediaType (MediaType p)

-- Another micro optimization - we precomupute
-- type from the media type...
matchedBy ∷ MediaType → MediaPattern → Boolean
matchedBy mt = do
  let
    mtt = mediaTypeType mt
  case _ of
    ProperMediaType pt → pt == mt
    AnySubtype mtt' → mtt == mtt'
    AnyMedia → true

