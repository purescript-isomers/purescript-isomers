module Isomers.HTTP.Request.Headers.Accept
  ( parse
  , module Exports
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split) as String
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (MediaPattern(..), matchedBy, mediaTypeType, mediaTypeSubtype, MediaTypeType(..), MediaTypeSubtype(..)) as Exports
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (MediaPattern)
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (parse) as MediaPattern

parse ∷ String → Array { pattern ∷ MediaPattern, q ∷ Maybe String }
parse h = do
  v ← String.split (String.Pattern ",") h
  case String.split (String.Pattern ";") v of
    [ m ] → [ { pattern: MediaPattern.parse m, q: Nothing } ]
    [ m, q ] → [ { pattern: MediaPattern.parse m, q: Just q } ]
    otherwise → []

