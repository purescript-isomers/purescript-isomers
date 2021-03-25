module Isomers.HTTP.Request.Interfaces where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Header (Header)
import Node.Stream (Readable) as Node.Stream
import Web.Fetch.Integrity (Integrity) as Fetch
import Web.Fetch.Referrer (Referrer) as Fetch
import Web.Fetch.ReferrerPolicy (ReferrerPolicy) as Fetch
import Web.Fetch.RequestBody (RequestBody) as Fetch
import Web.Fetch.RequestCache (RequestCache) as Fetch
import Web.Fetch.RequestCredentials (RequestCredentials) as Fetch
import Web.Fetch.RequestMode (RequestMode) as Fetch
import Web.Fetch.RequestRedirect (RequestRedirect) as Fetch

type Base body middleware =
  { body ∷ body
  , httpVersion ∷ String
  , method ∷ String
  , url ∷ String
  | middleware
  }

-- | TODO:
-- | I'm not sure if I'm able to easily carry this middleware parameter up
-- | in the context of our generic `Spec` machinery.
type Node middleware =
  Base
  { stream ∷ Node.Stream.Readable () }
  (headers ∷ Map HeaderName String | middleware)

type Web =
  Base
  Fetch.RequestBody
  ( headers ∷ Array Header
  , credentials :: Fetch.RequestCredentials
  , cache :: Fetch.RequestCache
  , mode :: Fetch.RequestMode
  , redirect ∷ Fetch.RequestRedirect
  , referrer :: Maybe Fetch.Referrer
  , referrerPolicy :: Fetch.ReferrerPolicy
  , integrity :: Fetch.Integrity
  )
