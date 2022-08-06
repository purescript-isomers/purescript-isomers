module Isomers.Node where

import Prelude

import Data.Argonaut (stringify)
import Isomers.Node.Request as Request
import Isomers.Request.Duplex as Request.Duplex
import Isomers.Request.Encodings as Request.Encodings
import Isomers.Runtime as Runtime

request :: Runtime.Node -> _
request node = do
  let
    prsBody = Request.readBodyString <<< Request.fromServerBody node
    stringBody = Request.Duplex.body Request.Encodings.StringBody prsBody
  { stringBody
  , jsonBody: \print parse ->
      Request.Duplex.as
        { print, parse, show: stringify }
        (Request.Duplex.json stringBody)
  }
