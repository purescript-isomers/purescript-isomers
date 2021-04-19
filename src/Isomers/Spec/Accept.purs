module Isomers.Spec.Accept where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract) as Comonad
import Data.Array (uncons) as Array
import Data.Foldable (foldl)
import Data.Homogeneous (class ToHomogeneousRow)
import Data.Homogeneous.Variant (Homogeneous, homogeneous') as Homogeneous.Variant
import Data.Lazy (force) as Lazy
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (expand, inj) as Variant
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.Contrib.Data.Variant (tag) as Contrib.Data.Variant
import Isomers.Contrib.Type.Eval.Foldable (Foldr')
import Isomers.Contrib.Type.Eval.Foldings (HomogeneousRow)
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
import Record.Extra (type (:::), SLProxy, SNil)
import Type.Eval (class Eval, Lift, kind TypeExpr)
import Type.Prelude (class IsSymbol, class TypeEquals, RProxy, SProxy(..), reflectSymbol)

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
  ∀ body ireq route oreq cts ivReq ovReq ls.
  Eval (HomogeneousRow Void cts) (RProxy ls) ⇒
  ToHomogeneousRow ls ireq ivReq ⇒
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
    prt' i = do
      let
        h ∷ Homogeneous.Variant.Homogeneous ls ireq
        h = Homogeneous.Variant.homogeneous' i
      Request.Duplex.Printer.header hAccept (Contrib.Data.Variant.tag i)
        <> (prt <<< Comonad.extract $ h)

    dst' ∷ Variant ivReq → route
    dst' i = do
      let
        h ∷ Homogeneous.Variant.Homogeneous ls ireq
        h = Homogeneous.Variant.homogeneous' i
      dst <<< Comonad.extract $ h
  Request.Accum (Request.Duplex prt' prs') dst'

responsePrinters :: forall lst rec. HFoldl ResponseContentTypeRecord {} lst rec => lst -> rec
responsePrinters lst = (hfoldl ResponseContentTypeRecord {} $ lst)

foreign import data DoSCons ∷ Type → TypeExpr → TypeExpr

instance evalDoSCons ∷
  (Eval t (SLProxy t')) ⇒
  Eval (DoSCons (SProxy h) t) (SLProxy (h ::: t'))

type ContentTypes cts
  = (Foldr' DoSCons (Lift (SLProxy SNil))) cts

-- | From a pair of request / response duplexes and a hlist of response codecs create a spec which
-- | labels everything by content types.
accumSpec ∷
  ∀ body res resLst route ireq oreq cts sl ivReq ovReq.
  HFoldl ResponseContentTypeRecord {} resLst res ⇒
  HMap ResponseContentType resLst cts ⇒
  Eval (HomogeneousRow Void cts) (RProxy sl) ⇒
  ToHomogeneousRow sl ireq ivReq ⇒
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
