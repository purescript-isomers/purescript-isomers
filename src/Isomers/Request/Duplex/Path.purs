module Isomers.Request.Duplex.Path where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, indexOf, length, split, take) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import JSURI (decodeURIComponent)
import Partial.Unsafe (unsafeCrashWith)

type Params = Array (String /\ String)

type Parts =
  { segments :: Array String
  , params :: Params
  , hash :: String
  }

parse :: String -> Parts
parse =
  splitAt (flip Tuple "") "#"
    >>> lmap splitPath
    >>> toRequestState
  where
  splitPath =
    splitAt (flip Tuple "") "?"
      >>> bimap splitSegments splitParams

  splitSegments =
    splitNonEmpty (String.Pattern "/")
      >>> case _ of
        [ "", "" ] -> [ "" ]
        xs -> map unsafeDecodeURIComponent xs

  unsafeDecodeURIComponent s = do
    case decodeURIComponent s of
      Just s -> s
      Nothing -> unsafeCrashWith ("URI decoding failed: " <> s)

  splitParams = splitNonEmpty (String.Pattern "&") >>> map splitKeyValue

  splitKeyValue = splitAt (flip Tuple "") "=" >>> bimap unsafeDecodeURIComponent unsafeDecodeURIComponent

  splitNonEmpty _ "" = []

  splitNonEmpty p s = String.split p s

  toRequestState (Tuple (Tuple segments params) h) = { segments, params, hash: h }

  splitAt k p str = case String.indexOf (String.Pattern p) str of
    Just ix -> Tuple (String.take ix str) (String.drop (ix + String.length p) str)
    Nothing -> k str
