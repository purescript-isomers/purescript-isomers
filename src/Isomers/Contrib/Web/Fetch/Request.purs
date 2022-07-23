module Isomers.Contrib.Web.Fetch.Request where

import Data.Argonaut (Json)
import Web.Fetch.RequestBody (RequestBody)

foreign import fromJson :: Json -> RequestBody
