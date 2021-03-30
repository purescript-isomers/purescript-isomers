module Isomers.Request
  ( module Duplex
  , module Types
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Maybe (fromMaybe)
import Data.Newtype (un)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Effect (Effect)
import Isomers.Contrib.Web.Fetch.Request (fromJson) as Contrib.Web.Fetch.Request
import Isomers.Request.Duplex (Duplex(..), Duplex', Parser(..), Printer(..)) as Duplex
import Isomers.Request.Types (ClientBody(..), ClientRequest)
import Isomers.Request.Types (ClientRequest, ClientBody(..), ServerRequest) as Types
import Web.Fetch.Headers (fromFoldable) as Fetch.Headers
import Web.Fetch.Request (Request, new) as Fetch
import Web.Fetch.Request (defaultOptions) as Fetch.Request
import Web.Fetch.RequestBody (empty, fromArrayBuffer, fromString) as Fetch.RequestBody

fetchRequest ∷ ClientRequest → Effect Fetch.Request
fetchRequest c = Fetch.new c.path opts
  where
    body = fromMaybe Fetch.RequestBody.empty $ c.body <#> case _ of
      ArrayBufferBody buff → Fetch.RequestBody.fromArrayBuffer buff
      JsonBody j → Contrib.Web.Fetch.Request.fromJson j
      StringBody cr → Fetch.RequestBody.fromString cr

    headers = Fetch.Headers.fromFoldable <<< map (lmap (un CaseInsensitiveString)) $ c.headers
    opts = Fetch.Request.defaultOptions { body = body, headers = headers, method = c.method }

