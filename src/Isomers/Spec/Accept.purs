module Isomers.Spec.Accept where

import Prelude

import Control.Alt ((<|>))
import Data.Array (uncons) as Array
import Data.Foldable (foldl)
import Data.Functor.Variant (FProxy, VariantF)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Tuple (lookup) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (case_, expand, inj, on) as Variant
import Data.Variant.Prefix (NilExpr)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.HTTP.Headers.Accept (MediaPattern(..), matches, parseHeader, printMediaPattern) as Accept
import Isomers.HTTP.Headers.Accept (MediaPattern)
import Isomers.HTTP.Response (Duplex) as Response
import Isomers.HTTP.Response (OkF)
import Isomers.Request (Duplex(..), Duplex') as Request
import Isomers.Spec.Spec (Spec(..))
import Network.HTTP.Types (hAccept)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Record (insert) as Record
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Request.Duplex.Parser
import Request.Duplex.Parser (RequestParser, runRequestParser)
import Request.Duplex.Parser (RouteError(..), RequestParser(..)) as RequestDuplex.Parser
import Request.Duplex.Printer (RequestPrinter)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow)
import Type.Prelude (class IsSymbol, class TypeEquals, RProxy, SProxy(..), reflectSymbol)

newtype Accept v
  = Accept v

foreign import data ResponseContentTypeStep ∷ Type → Type → TypeExpr → TypeExpr

type ResponseContentType res
  = (FoldrWithIndex ResponseContentTypeStep NilExpr <<< FromRow) (RProxy res)

instance evalResponseContentTypeStepMatch ∷
  Eval (ResponseContentTypeStep (SProxy "ok") (FProxy (OkF contentType)) acc) (SProxy contentType)
else instance evalResponseContentTypeStepNoMatch ∷
  (Eval acc acc') ⇒
  Eval (ResponseContentTypeStep idx t acc) acc'

-- | Given a hlist of responses create a record with content types as labels.
data ResponseRecFolding
  = ResponseRecFolding

instance foldingResponseRecFolding ∷
  ( Eval (ResponseContentType res) (SProxy contentType)
  , IsSymbol contentType
  , Row.Lacks contentType response
  , Row.Cons contentType (Response.Duplex (VariantF res i) (VariantF res o)) response response'
  ) ⇒
  Folding
    ResponseRecFolding
    { | response }
    (Response.Duplex (VariantF res i) (VariantF res o))
    { | response' } where
  folding _ acc r = Record.insert (SProxy ∷ SProxy contentType) r acc

data RequestParserFolding r o
  = RequestParserFolding (RequestParser (r → o))

-- | I'm not sure if this is worth complication
-- | but this folding is done once and produces a function.
-- | We could do it on every request but I had an
-- | __intuition__ that this approach can be a bit
-- | faster.
instance foldingRequestParserFolding ∷
  ( IsSymbol contentType
  , Row.Cons contentType req () ctRow
  , Row.Union vReq ctRow vReq'
  , Row.Cons contentType req vReq vReq'
  ) ⇒
  Folding
    (RequestParserFolding r req)
    (MediaPattern → RequestParser (r → Variant vReq))
    (SProxy contentType)
    (MediaPattern → RequestParser (r → Variant vReq')) where
  folding (RequestParserFolding prs) vprs ct = do
    let
      mt = MediaType (reflectSymbol ct)

      prs' ∷ RequestParser (r → Variant vReq')
      prs' = map (Variant.inj ct) <$> prs

      vprs' ∷ MediaPattern → RequestParser (r → Variant vReq')
      vprs' mp = map Variant.expand <$> vprs mp
    \mp →
      vprs' mp <|> parserBuilder mt prs' mp

parserBuilder ∷ ∀ a. MediaType → RequestParser a → MediaPattern → RequestParser a
parserBuilder mediaType@(MediaType mtStr) prs = do
  let
    match = (mediaType `Accept.matches` _)
  \pattern →
    if match pattern then
      prs
    else
      Request.Duplex.Parser.Chomp \_ →
        Request.Duplex.Parser.Fail (Request.Duplex.Parser.Expected mtStr $ Accept.printMediaPattern pattern)


newtype RequestPrinterFolding i
  = RequestPrinterFolding (i → RequestPrinter)

instance foldingRequestPrinterFolding ∷
  ( IsSymbol contentType
  , Row.Cons contentType req vReq_ vReq
  ) ⇒
  Folding
    (RequestPrinterFolding req)
    (Variant vReq_ → RequestPrinter)
    (SProxy contentType)
    (Variant vReq → RequestPrinter) where
  folding (RequestPrinterFolding prt) vptr ct = Variant.on ct prt vptr

-- | Extract content type symbol from `Response` type.
-- | We use it to extract a heterogeneous list of proxies
-- | from a list of response codecs.
-- | This is an initial step for `request` fold.
data ResponseContentTypesMapping
  = ResponseContentTypesMapping

instance mappingContentTypesMapping ∷
  ( Eval (ResponseContentType res) (SProxy contentType)
  , TypeEquals f (Response.Duplex (VariantF res i) (VariantF res o))
  , IsSymbol contentType
  ) ⇒
  Mapping
    ResponseContentTypesMapping
    f
    (SProxy contentType) where
  mapping _ _ = SProxy ∷ SProxy contentType

-- | Given an "inner" request duplex and a heterogeneous list of labels.
-- | We create a request duplex for homogeneous variant which
-- | has content types as labels.
request ∷
  ∀ req cts r vReq.
  HFoldl
    (RequestParserFolding r req)
    (MediaPattern → RequestParser (r → Variant ()))
    cts
    (MediaPattern → RequestParser (r → Variant vReq)) ⇒
  HFoldl
    (RequestPrinterFolding req)
    (Variant () → RequestPrinter)
    cts
    (Variant vReq → RequestPrinter) ⇒
  Request.Duplex r req req →
  cts →
  Request.Duplex' r (Accept (Variant vReq))
request (Request.Duplex prt prs) cts = do
  let
    buildParser ∷ MediaPattern → RequestParser (r → (Variant vReq))
    buildParser = do
      let
        noMatch ∷ RequestParser (r → Variant ())
        noMatch =
          Request.Duplex.Parser.Chomp \_ →
            Request.Duplex.Parser.Fail (RequestDuplex.Parser.Expected "Provide a better error" " for non matching content type against Accept header value")
      hfoldl
        (RequestParserFolding prs ∷ RequestParserFolding r req)
        (const noMatch ∷ MediaPattern → RequestParser (r → Variant ()))
        cts

    prs' = RequestDuplex.Parser.Chomp chomp
      where
      chomp state@{ headers } = do
        let
          parseMediaPatternParsers = map (buildParser <<< _.pattern) <$> Accept.parseHeader

          ps = case (parseMediaPatternParsers <$> Tuple.lookup hAccept headers) >>= Array.uncons of
            Just ht → ht
            Nothing → { head: buildParser Accept.AnyMedia, tail: [] }
        runRequestParser state (foldl (<|>) ps.head ps.tail)

    prt' ∷ Accept (Variant vReq) → RequestPrinter
    prt' (Accept i) = hfoldl (RequestPrinterFolding prt) (Variant.case_ ∷ Variant () → RequestPrinter) cts i
  Request.Duplex prt' (map Accept <$> prs')

response :: forall lst rec. HFoldl ResponseRecFolding {} lst rec => lst -> Accept rec
response lst = (Accept <<< hfoldl ResponseRecFolding {} $ lst)

-- | From a pair of request duplexes and hlist of response codecs create a spec which
-- | labels everything by content types and wraps in `Accept`
spec ∷
  ∀ r res resLst req cts vReq.
  HMap ResponseContentTypesMapping resLst cts ⇒
  HFoldl ResponseRecFolding {} resLst res ⇒
  HFoldl
    (RequestParserFolding r req)
    (MediaPattern → RequestParser (r → Variant ()))
    cts
    (MediaPattern → RequestParser (r → Variant vReq)) ⇒
  HFoldl
    (RequestPrinterFolding req)
    (Variant () → RequestPrinter)
    cts
    (Variant vReq → RequestPrinter) ⇒
  (Request.Duplex r req req /\ resLst) →
  Spec r (Accept (Variant vReq)) (Accept res)
spec (req /\ res) = do
  let
    cts ∷ cts
    cts = hmap ResponseContentTypesMapping res
  Spec
    { request: request req cts
    , response: response res
    }
