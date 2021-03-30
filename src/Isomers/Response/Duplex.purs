module Isomers.Response.Duplex
  ( module Type
  , asJson
  , header
  , json
  , reqHeader
  , status
  , string
  , withHeaderValue
  , withStatus
  )
  where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Isomers.Response.Duplex.Parser (ParsingError(..), fromJson, header, json, reqHeader, status, statusEquals, string) as Parser
import Isomers.Response.Duplex.Printer (header, json, reqHeader, status, string) as Printer
import Isomers.Response.Duplex.Type (Duplex(..), Duplex')
import Isomers.Response.Duplex.Type (Duplex(..), Duplex') as Type
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types (Status) as HTTP.Types

status ∷ Duplex' HTTP.Types.Status
status = Duplex Printer.status Parser.status

header ∷ HeaderName → Duplex' (Maybe String)
header headerName = Duplex (Printer.header headerName) (Parser.header headerName)

reqHeader ∷ HeaderName → Duplex' String
reqHeader headerName = Duplex (Printer.reqHeader headerName) (Parser.reqHeader headerName)

withHeaderValue ∷ ∀ i o. HeaderName → String → Duplex i o → Duplex i o
withHeaderValue hn@(CaseInsensitiveString str) expected (Duplex prt prs) = Duplex
  (\i → Printer.reqHeader hn str <> prt i)
  ( Parser.reqHeader hn >>= \got → do
      when (expected /= got) do
        throwError (Parser.Expected (str <> ":" <> expected) got)
      prs
  )

withStatus ∷ ∀ a b. HTTP.Types.Status → Duplex a b → Duplex a b
withStatus s (Duplex prt prs) = Duplex prt' prs'
  where
    prt' i = Printer.status s <> prt i
    prs' = Parser.statusEquals s *> prs

json ∷ Duplex' Json
json = Duplex Printer.json Parser.json

string ∷ Duplex' String
string = Duplex Printer.string Parser.string

asJson ∷ ∀ i o. (i → Json) → (Json → Either String o) → Duplex i o
asJson f g = Duplex (Printer.json <<< f) (Parser.fromJson g)

