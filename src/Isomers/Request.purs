module Isomers.Request
  ( module Exports
  , toFetchRequest
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Maybe (fromMaybe)
import Data.Newtype (un)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Effect (Effect)
import Isomers.Contrib.Web.Fetch.Request (fromJson) as Contrib.Web.Fetch.Request
import Isomers.Request.Duplex (body) as Exports
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError) as Exports
import Isomers.Request.Duplex.Printer (Printer(..)) as Exports
import Isomers.Request.Duplex.Type (Duplex(..), Duplex', print, prefix, path, parse, parse') as Exports
import Isomers.Request.Types (ClientBody(..), ClientRequest)
import Isomers.Request.Types (ClientRequest, ClientBody(..), ServerRequest) as Exports
import Web.Fetch.Headers (fromFoldable) as Fetch.Headers
import Web.Fetch.Request (Request, new) as Fetch
import Web.Fetch.Request (defaultOptions) as Fetch.Request
import Web.Fetch.RequestBody (empty, fromArrayBuffer, fromString) as Fetch.RequestBody

toFetchRequest ∷ ClientRequest → Effect Fetch.Request
toFetchRequest c = Fetch.new c.path opts
  where
    body = fromMaybe Fetch.RequestBody.empty $ c.body <#> case _ of
      ArrayBufferBody buff → Fetch.RequestBody.fromArrayBuffer buff
      JsonBody j → Contrib.Web.Fetch.Request.fromJson j
      StringBody cr → Fetch.RequestBody.fromString cr

    headers = Fetch.Headers.fromFoldable <<< map (lmap (un CaseInsensitiveString)) $ c.headers
    opts = Fetch.Request.defaultOptions { body = body, headers = headers, method = c.method }



