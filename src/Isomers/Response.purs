module Isomers.Response
  ( module Exports
  , parse
  , print
  ) where

import Data.Either (Either)
import Effect.Aff (Aff)
import Isomers.Response.Duplex (Duplex(..), Duplex') as Exports
import Isomers.Response.Duplex.Parser (ParsingError)
import Isomers.Response.Duplex.Parser (run) as Parser
import Isomers.Response.Duplex.Printer (run) as Printer
import Isomers.Response.Duplex.Type (Duplex(..))
import Isomers.Response.Encodings (ClientResponse, ServerResponse) as Encodings
import Isomers.Response.Okayish.Type (Okayish) as Exports
import Isomers.Response.Okayish.Duplexes (OkayishDuplex, OkayishDuplex') as Exports
import Isomers.Response.Raw (RawServer, RawClient, RawDuplex') as Exports

parse ∷ ∀ ct i o. Duplex ct i o → Encodings.ClientResponse → Aff (Either ParsingError o)
parse (Duplex _ prs) = Parser.run prs

print ∷ ∀ ct i o. Duplex ct i o → i → Encodings.ServerResponse
print (Duplex prt _) i = Printer.run (prt i)

