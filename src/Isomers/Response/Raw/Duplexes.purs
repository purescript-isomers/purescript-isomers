module Isomers.Response.Raw.Duplexes where

import Prelude

import Data.Foldable (foldMap)
import Data.Tuple (uncurry)
import Isomers.HTTP.ContentTypes (HtmlMime, TextMime)
import Isomers.Response.Duplex (Duplex(..))
import Isomers.Response.Duplex.Encodings (_string)
import Isomers.Response.Duplex.Parser (headers, readBody, status) as Parser
import Isomers.Response.Duplex.Printer (reqHeader, status, string) as Printer
import Isomers.Response.Raw.Types (RawClient(..), RawServer(..))
import Isomers.Response.Types (HtmlString(..))

type RawDuplex ct i o = Duplex ct (RawServer i) (RawClient o)
type RawDuplex' ct a = RawDuplex ct a a

string :: RawDuplex' TextMime String
string = Duplex prt prs
  where
    prs = do
      status ← Parser.status
      body ← Parser.readBody _string
      headers ← Parser.headers
      pure $ RawClient { body, headers, status }

    prt (RawServer { body, headers, status }) =
        foldMap (uncurry Printer.reqHeader) headers <> Printer.status status <> Printer.string body


html :: RawDuplex' HtmlMime HtmlString
html = Duplex prt prs
  where
    prs = do
      status ← Parser.status
      body ← Parser.readBody _string
      headers ← Parser.headers
      pure $ RawClient { body: HtmlString body, headers, status }

    prt (RawServer { body: HtmlString body, headers, status }) =
        foldMap (uncurry Printer.reqHeader) headers <> Printer.status status <> Printer.string body

