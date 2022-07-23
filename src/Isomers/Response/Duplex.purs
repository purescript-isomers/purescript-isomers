module Isomers.Response.Duplex
  ( module Exports
  , asJson
  , header
  , javascript
  , html
  , json
  , reqHeader
  , status
  , string
  , withHeaderValue
  , withStatus
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Isomers.HTTP.ContentTypes (HtmlMime, JavascriptMime, JsonMime, TextMime)
import Isomers.Response.Duplex.Parser
  ( ParsingError(..)
  , fromJson
  , header
  , json
  , reqHeader
  , status
  , statusEquals
  , string
  , withContentType
  ) as Parser
import Isomers.Response.Duplex.Printer (Printer(..)) as Exports
import Isomers.Response.Duplex.Printer (header, json, reqHeader, status, string) as Printer
import Isomers.Response.Duplex.Type (Duplex(..), Duplex')
import Isomers.Response.Duplex.Type (Duplex(..), Duplex') as Exports
import Isomers.Response.Types (HtmlString, JavascriptString)
import Network.HTTP.Types (HeaderName, hContentType)
import Network.HTTP.Types (Status) as HTTP.Types
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

status :: forall ct. Duplex' ct HTTP.Types.Status
status = Duplex Printer.status Parser.status

header :: forall ct. HeaderName -> Duplex' ct (Maybe String)
header headerName = Duplex (Printer.header headerName) (Parser.header headerName)

reqHeader :: forall ct. HeaderName -> Duplex' ct String
reqHeader headerName = Duplex (Printer.reqHeader headerName) (Parser.reqHeader headerName)

withHeaderValue :: forall ct i o. HeaderName -> String -> Duplex ct i o -> Duplex ct i o
withHeaderValue hn@(CaseInsensitiveString str) expected (Duplex prt prs) = Duplex
  (\i -> Printer.reqHeader hn expected <> prt i)
  ( Parser.reqHeader hn >>= \got -> do
      when (expected /= got) do
        throwError (Parser.Expected (str <> ":" <> expected) got)
      prs
  )

withContentType :: forall ct i o. IsSymbol ct => Duplex ct i o -> Duplex ct i o
withContentType (Duplex prt prs) = Duplex prt' prs'
  where
  ct = reflectSymbol (Proxy :: Proxy ct)
  prt' i = Printer.reqHeader hContentType ct <> prt i
  prs' = Parser.withContentType ct prs

withStatus :: forall ct i o. HTTP.Types.Status -> Duplex ct i o -> Duplex ct i o
withStatus s (Duplex prt prs) = Duplex prt' prs'
  where
  prt' i = Printer.status s <> prt i
  prs' = Parser.statusEquals s *> prs

string :: Duplex' TextMime String
string = withContentType (Duplex Printer.string Parser.string)

json :: Duplex' JsonMime Json
json = withContentType (Duplex Printer.json Parser.json)

asJson :: forall i o. (i -> Json) -> (Json -> Either String o) -> Duplex JsonMime i o
asJson f g = withContentType $ Duplex (Printer.json <<< f) (Parser.fromJson g)

javascript :: Duplex' JavascriptMime JavascriptString
javascript = withContentType (_Newtype $ Duplex Printer.string Parser.string)

html :: Duplex' HtmlMime HtmlString
html = withContentType (_Newtype $ Duplex Printer.string Parser.string)
