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
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.HTTP.Request.Headers.Accept (MediaPattern(..), parse) as Accept
import Isomers.HTTP.Request.Headers.Accept (MediaPattern)
import Isomers.HTTP.Request.Headers.Accept.MediaPattern (matchedBy, print) as Accept.MediaPattern
import Isomers.Request (Accum(..), Duplex(..), Parser, Printer) as Request
import Isomers.Request.Duplex.Parser (Parser(..), ParsingError(..), Result(..)) as Request.Duplex.Parser
import Isomers.Request.Duplex.Parser (runParser) as Request.Parser
import Isomers.Request.Duplex.Printer (header) as Request.Duplex.Printer
import Isomers.Response (Duplex) as Response
import Isomers.Spec.Types (AccumSpec(..))
import Network.HTTP.Types (hAccept)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Record (insert) as Record
import Type.Prelude (class IsSymbol, class TypeEquals, SProxy(..), reflectSymbol)

-- | Fold from `Response.Duplex`es a `Record`
-- | with according content types as labels.
data ResponseContentTypeRecord
  = ResponseContentTypeRecord

instance foldingResponseContentTypeRecord ∷
  ( IsSymbol ct
  , Row.Lacks ct acc
  , Row.Cons ct (Response.Duplex ct i o) acc acc'
  ) ⇒
  Folding
    ResponseContentTypeRecord
    { | acc }
    (Response.Duplex ct i o)
    { | acc' } where
  folding _ acc r = Record.insert (SProxy ∷ SProxy ct) r acc

-- | Extract content type symbol from `Response` type.
-- | We use it to extract a heterogeneous list of proxies
-- | from a list of response codecs.
-- | This is an initial step for `request` fold.
data ResponseContentType
  = ResponseContentType

instance mappingResponseContentType ∷
  (TypeEquals f (Response.Duplex ct i o), IsSymbol ct) ⇒
  Mapping ResponseContentType f (SProxy ct) where
  mapping _ _ = SProxy ∷ SProxy ct

-- | Fold over a `HList` of content types encoding `Symbol`s to
-- | build a media pattern matching parser which outputs an
-- | `Variant` constructor. Variant has provided content
-- | types as labels.
-- |
-- | We don't return a `Variant` value directly from here
-- | because it should be faster to use constructed function
-- | then to do `hfold` on every request.
data RequestMediaPatternParser body r o
  = RequestMediaPatternParser (Request.Parser body (r → o))

instance foldingRequestMediaPatternParser ∷
  ( IsSymbol contentType
  , Row.Cons contentType req () ctRow
  , Row.Union vReq ctRow vReq'
  , Row.Cons contentType req vReq vReq'
  ) ⇒
  Folding
    (RequestMediaPatternParser body r req)
    (MediaPattern → Request.Parser body (r → Variant vReq))
    (SProxy contentType)
    (MediaPattern → Request.Parser body (r → Variant vReq')) where
  folding (RequestMediaPatternParser prs) vprs ct = do
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
    match p = mediaType `Accept.MediaPattern.matchedBy` p
  \pattern →
    if match pattern then
      prs
    else
      Request.Duplex.Parser.Chomp \_ →
        pure
          $ Request.Duplex.Parser.Fail (Request.Duplex.Parser.Expected mtStr $ Accept.MediaPattern.print pattern)


-- | Content types defined by user are not properly sorted.
-- | But we don't have to worry about this in I think as
-- | we doesn't expose our inner `Variant.Homogeneous` outside
-- | of the scope of this function.
-- |
-- | Given an "inner" request duplex and a heterogeneous list of labels
-- | we create a request duplex for homogeneous variant which
-- | has content types as labels.
-- |
-- | XXX: I'm not sure why "`class` alias" is not fully working
-- | and I have to put here `RowSList` too.
requestAccum ∷
  ∀ body ireq route oreq cts ivReq ovReq.
  HFoldl (ExtractRoute ireq route) (Variant () → route) cts (Variant ivReq → route) ⇒
  HFoldl (RequestPrinter ireq) (Variant () → Request.Printer) cts (Variant ivReq → Request.Printer) ⇒
  HFoldl
    (RequestMediaPatternParser body route oreq)
    (MediaPattern → Request.Parser body (route → Variant ()))
    cts
    (MediaPattern → Request.Parser body (route → Variant ovReq)) ⇒
  Request.Accum body route ireq oreq →
  cts →
  Request.Accum body route (Variant ivReq) (Variant ovReq)
requestAccum (Request.Accum (Request.Duplex prt prs) dst) cts = do
  let
    buildParser ∷ MediaPattern → Request.Parser body (route → (Variant ovReq))
    buildParser = do
      let
        -- | TODO: Improve reporting - replace `unsafeStringify` with something sane ;-)
        noMatch ∷ MediaPattern → Request.Parser body (route → Variant ())
        noMatch mp =
          Request.Duplex.Parser.Chomp \_ →
            pure
              $ Request.Duplex.Parser.Fail
              $ Request.Duplex.Parser.Expected (unsafeStringify cts)
              $ unsafeStringify mp
      hfoldl
        (RequestMediaPatternParser prs ∷ RequestMediaPatternParser body route oreq)
        (noMatch ∷ MediaPattern → Request.Parser body (route → Variant ()))
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

    prt' ∷ Variant ivReq → Request.Printer
    prt' i = hfoldl (RequestPrinter prt) (Variant.case_ ∷ Variant () → Request.Printer) cts i

    dst' ∷ Variant ivReq → route
    dst' i = hfoldl (ExtractRoute (dst ∷ ireq → route)) (Variant.case_ ∷ Variant () → route) cts i
  Request.Accum (Request.Duplex prt' prs') dst'

responsePrinters :: forall lst rec. HFoldl ResponseContentTypeRecord {} lst rec => lst -> rec
responsePrinters lst = (hfoldl ResponseContentTypeRecord {} $ lst)

-- | From a pair of request / response duplexes and a hlist of response codecs create a spec which
-- | labels everything by content types.
accumSpec ∷
  ∀ body res resLst route ireq oreq cts ivReq ovReq.
  HFoldl ResponseContentTypeRecord {} resLst res ⇒
  HMap ResponseContentType resLst cts ⇒
  HFoldl (ExtractRoute ireq route) (Variant () → route) cts (Variant ivReq → route) ⇒
  HFoldl (RequestPrinter ireq) (Variant () → Request.Printer) cts (Variant ivReq → Request.Printer) ⇒
  HFoldl
    (RequestMediaPatternParser body route oreq)
    (MediaPattern → Request.Parser body (route → Variant ()))
    cts
    (MediaPattern → Request.Parser body (route → Variant ovReq)) ⇒
  (Request.Accum body route ireq oreq /\ resLst) →
  AccumSpec body route (Variant ivReq) (Variant ovReq) res
accumSpec (req /\ res) = do
  let
    cts ∷ cts
    cts = hmap ResponseContentType res
  AccumSpec
    { request: requestAccum req cts
    , response: responsePrinters res
    }

-- | This folding builds a request printer for a homogeneous
-- | `Variant` from a single printing function.
-- | I should just use _homogeneous_ here.
newtype RequestPrinter i
  = RequestPrinter (i → Request.Printer)

instance foldingRequestPrinter ∷
  ( IsSymbol contentType
  , Row.Cons contentType req vReq_ vReq
  ) ⇒
  Folding
    (RequestPrinter req)
    (Variant vReq_ → Request.Printer)
    (SProxy contentType)
    (Variant vReq → Request.Printer) where
  folding (RequestPrinter prt) vprt ct =
    Variant.on ct (append (Request.Duplex.Printer.header hAccept (reflectSymbol ct)) <<< prt) vprt

-- | `Request.Accum` contains an deconstruting helper function
-- | which provide a way to compose partial printers.
-- | We fold over this access homogeneous `Variant` (I wasn't able to use
-- | `purescript-homogeneous` because of some inference problems)
-- | to build this extraction function.
data ExtractRoute ireq route
  = ExtractRoute (ireq → route)

instance foldingExtractRoute ∷
  ( IsSymbol contentType
  , Row.Cons contentType ireq vReq_ vReq
  ) ⇒
  Folding
    (ExtractRoute ireq route)
    (Variant vReq_ → route)
    (SProxy contentType)
    (Variant vReq → route) where
  folding (ExtractRoute route) vprt ct = Variant.on ct route vprt

