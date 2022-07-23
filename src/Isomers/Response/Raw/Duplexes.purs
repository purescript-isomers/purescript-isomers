module Isomers.Response.Raw.Duplexes where

import Prelude

import Data.Foldable (foldMap)
import Data.Tuple (uncurry)
import Isomers.HTTP.ContentTypes (HtmlMime, TextMime)
import Isomers.Response.Duplex (Duplex(..))
import Isomers.Response.Duplex.Parser (headers, readBody, status, withContentType) as Parser
import Isomers.Response.Duplex.Printer (reqHeader, status, string) as Printer
import Isomers.Response.Encodings (_string)
import Isomers.Response.Raw.Types (RawClient(..), RawServer(..))
import Isomers.Response.Types (HtmlString(..))
import Network.HTTP.Types (hContentType)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

type RawDuplex ct i o = Duplex ct (RawServer i) (RawClient o)
type RawDuplex' ct a = RawDuplex ct a a

withContentType :: forall ct i o. IsSymbol ct => RawDuplex ct i o -> RawDuplex ct i o
withContentType (Duplex prt prs) = Duplex prt' prs'
  where
  ct = reflectSymbol (Proxy :: Proxy ct)
  prt' i = Printer.reqHeader hContentType ct <> prt i
  prs' = Parser.withContentType ct prs

string :: RawDuplex' TextMime String
string = withContentType $ Duplex prt prs
  where
  prs = do
    status <- Parser.status
    body <- Parser.readBody _string
    headers <- Parser.headers
    pure $ RawClient { body, headers, status }

  prt (RawServer { body, headers, status }) =
    foldMap (uncurry Printer.reqHeader) headers <> Printer.status status <> Printer.string body

html :: RawDuplex' HtmlMime HtmlString
html = withContentType $ Duplex prt prs
  where
  prs = do
    status <- Parser.status
    body <- Parser.readBody _string
    headers <- Parser.headers
    pure $ RawClient { body: HtmlString body, headers, status }

  prt (RawServer { body: HtmlString body, headers, status }) =
    foldMap (uncurry Printer.reqHeader) headers <> Printer.status status <> Printer.string body

