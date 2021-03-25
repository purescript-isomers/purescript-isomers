module Isomers.Contrib.Web.Fetch where

import Data.Argonaut (Json)
import Effect (Effect)
import Web.Fetch.Response (Response)
import Web.Promise (Promise)

foreign import json ∷ Response → Effect (Promise Json)

-- foreign import formData ∷ Response → Effect (Promise FormData)

