module Isomers.Spec.Accept where

import Prelude

import Control.Alt ((<|>))
import Data.Array (uncons) as Array
import Data.Foldable (foldl)
import Data.Lazy (force) as Lazy
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (case_, expand, inj, on) as Variant
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.HTTP.Request.Headers.Accept (MediaPattern(..), parse) as Accept
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (matchedBy, print) as Accept.MediaPattern
import Isomers.Request (Duplex(..), Duplex', Parser, Printer) as Request
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError(..), Result(..)) as Request.Duplex.Parser
import Isomers.Request.Duplex.Parser (runParser) as Request.Parser
import Isomers.Request.Duplex.Printer (header) as Request.Duplex.Printer
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Type (Spec(..))
import Network.HTTP.Types (hAccept)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Record (insert) as Record
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy(..), reflectSymbol)

newtype Accept v = Accept v

-- | Given a hlist of responses create a record with content types as labels.
data ResponseRecordStep
  = ResponseRecordStep

instance foldingResponseRecordStep ∷
  ( IsSymbol ct
  , Row.Lacks ct response
  , Row.Cons ct (Response.Duplex ct i o) response response'
  ) ⇒
  Folding
    ResponseRecordStep
    { | response }
    (Response.Duplex ct i o)
    { | response' } where
  folding _ acc r = Record.insert (SProxy ∷ SProxy ct) r acc

data RequestParserFolding body r o
  = RequestParserFolding (Request.Parser body (r → o))

-- | I'm not sure if this is worth complication
-- | but this folding is done once and produces a function.
-- | We could do it on every request but I had an
-- | "intuition" that this approach can be a bit
-- | faster.
instance foldingRequestParserFolding ∷
  ( IsSymbol contentType
  , Row.Cons contentType req () ctRow
  , Row.Union vReq ctRow vReq'
  , Row.Cons contentType req vReq vReq'
  ) ⇒
  Folding
    (RequestParserFolding body r req)
    (MediaPattern → Request.Parser body (r → Variant vReq))
    (SProxy contentType)
    (MediaPattern → Request.Parser body (r → Variant vReq')) where
  folding (RequestParserFolding prs) vprs ct = do
    let
      mt = MediaType (reflectSymbol ct)

      prs' ∷ Request.Parser body (r → Variant vReq')
      prs' = map (Variant.inj ct) <$> prs

      vprs' ∷ MediaPattern → Request.Parser body (r → Variant vReq')
      vprs' mp = map Variant.expand <$> vprs mp
    \mp →
      vprs' mp <|> parserBuilder mt prs' mp

parserBuilder ∷ ∀ a body. MediaType → Request.Parser body a → MediaPattern → Request.Parser body a
parserBuilder mediaType@(MediaType mtStr) prs = do
  let
    match = (mediaType `Accept.MediaPattern.matchedBy` _)
  \pattern →
    if match pattern then
      prs
    else
      Request.Duplex.Parser.Chomp \_ →
        pure
          $ Request.Duplex.Parser.Fail (Request.Duplex.Parser.Expected mtStr $ Accept.MediaPattern.print pattern)

newtype RequestPrinterFolding i
  = RequestPrinterFolding (i → Request.Printer)

instance foldingRequestPrinterFolding ∷
  ( IsSymbol contentType
  , Row.Cons contentType req vReq_ vReq
  ) ⇒
  Folding
    (RequestPrinterFolding req)
    (Variant vReq_ → Request.Printer)
    (SProxy contentType)
    (Variant vReq → Request.Printer) where
  folding (RequestPrinterFolding prt) vprt ct = Variant.on ct (append (Request.Duplex.Printer.header hAccept (reflectSymbol ct)) <<< prt) vprt

-- | Extract content type symbol from `Response` type.
-- | We use it to extract a heterogeneous list of proxies
-- | from a list of response codecs.
-- | This is an initial step for `request` fold.
data ResponseContentTypesMapping
  = ResponseContentTypesMapping

instance mappingContentTypesMapping ∷
  ( TypeEquals f (Response.Duplex ct i o)
  , IsSymbol ct
  ) ⇒
  Mapping
    ResponseContentTypesMapping
    f
    (SProxy ct) where
  mapping _ _ = SProxy ∷ SProxy ct

-- | Given an "inner" request duplex and a heterogeneous list of labels.
-- | We create a request duplex for homogeneous variant which
-- | has content types as labels.
request ∷
  ∀ body req cts r vReq.
  HFoldl
    (RequestParserFolding body r req)
    (MediaPattern → Request.Parser body (r → Variant ()))
    cts
    (MediaPattern → Request.Parser body (r → Variant vReq)) ⇒
  HFoldl
    (RequestPrinterFolding req)
    (Variant () → Request.Printer)
    cts
    (Variant vReq → Request.Printer) ⇒
  Request.Duplex body r req req →
  cts →
  Request.Duplex' body r (Variant vReq)
request (Request.Duplex prt prs) cts = do
  let
    buildParser ∷ MediaPattern → Request.Parser body (r → (Variant vReq))
    buildParser = do
      let
        noMatch ∷ Request.Parser body (r → Variant ())
        noMatch =
          Request.Duplex.Parser.Chomp \_ →
            pure
              $ Request.Duplex.Parser.Fail (Request.Duplex.Parser.Expected "Provide a better error" " for non matching content type against Accept header value")
      hfoldl
        (RequestParserFolding prs ∷ RequestParserFolding body r req)
        (const noMatch ∷ MediaPattern → Request.Parser body (r → Variant ()))
        cts

    prs' = Request.Duplex.Parser.Chomp chomp
      where
      chomp state@{ headers } = do
        let
          parseMediaPatternParsers = map (buildParser <<< _.pattern) <$> Accept.parse

          ps = case (parseMediaPatternParsers <$> (Map.lookup hAccept $ Lazy.force headers)) >>= Array.uncons of
            Just ht → ht
            Nothing → { head: buildParser Accept.AnyMedia, tail: [] }
        Request.Parser.runParser state (foldl (<|>) ps.head ps.tail)

    prt' ∷ Variant vReq → Request.Printer
    prt' i = hfoldl (RequestPrinterFolding prt) (Variant.case_ ∷ Variant () → Request.Printer) cts i
  Request.Duplex prt' prs'

response :: forall lst rec. HFoldl ResponseRecordStep {} lst rec => lst -> rec
response lst = (hfoldl ResponseRecordStep {} $ lst)

-- | From a pair of request duplexes and hlist of response codecs create a spec which
-- | labels everything by content types and wraps in `Accept`
spec ∷
  ∀ body r res resLst req cts vReq.
  HMap ResponseContentTypesMapping resLst cts ⇒
  HFoldl ResponseRecordStep {} resLst res ⇒
  HFoldl
    (RequestParserFolding body r req)
    (MediaPattern → Request.Parser body (r → Variant ()))
    cts
    (MediaPattern → Request.Parser body (r → Variant vReq)) ⇒
  HFoldl
    (RequestPrinterFolding req)
    (Variant () → Request.Printer)
    cts
    (Variant vReq → Request.Printer) ⇒
  (Request.Duplex body r req req /\ resLst) →
  Spec body r (Variant vReq) res
spec (req /\ res) = do
  let
    cts ∷ cts
    cts = hmap ResponseContentTypesMapping res
  Spec
    { request: request req cts
    , response: response res
    }
