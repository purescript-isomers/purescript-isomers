module Isomers.HTTP.Request.Headers.Accept
  ( parse
  , module MediaPattern
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split) as String
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (MediaPattern(..), matchedBy, mediaTypeType, mediaTypeSubtype, MediaTypeType(..), MediaTypeSubtype(..)) as MediaPattern
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (MediaPattern, parse) as MP

parse ∷ String → Array { pattern ∷ MP.MediaPattern, q ∷ Maybe String }
parse h = do
  v ← String.split (String.Pattern ",") h
  case String.split (String.Pattern ";") v of
    [ m ] → [ { pattern: MP.parse m, q: Nothing } ]
    [ m, q ] → [ { pattern: MP.parse m, q: Just q } ]
    otherwise → []

