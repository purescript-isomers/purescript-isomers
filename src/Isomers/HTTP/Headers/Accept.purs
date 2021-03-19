module Isomers.HTTP.Headers.Accept where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (defer) as Control.Lazy
import Data.Foldable (foldMap)
import Data.Functor.Variant (FProxy, VariantF)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (ala)
import Data.Profunctor.Join (Join(..)) as Profunctor
import Data.String (Pattern(..), split) as String
import Data.Tuple (lookup) as Tuple
import Data.Variant (Variant)
import Data.Variant (case_, inj, on) as Variant
import Data.Variant.Prefix (NilExpr)
import Effect.Aff (Aff)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Isomers.Contrib.Type.Eval.Foldable (Foldr')
import Isomers.HTTP.Response (Duplex) as Response
import Isomers.HTTP.Response (OkF)
import Network.HTTP.Types (hAccept)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (Cons) as RL
import Record (insert) as Record
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Request.Duplex.Parser
import Request.Duplex.Parser (RequestParser, runRequestParser)
import Request.Duplex.Parser (RouteError(..), RequestParser(..)) as RequestDuplex.Parser
import Request.Duplex.Printer (RequestPrinter)
import Type.Equality (to) as Type.Equality
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy, RProxy, SProxy(..), reflectSymbol)

newtype MediaTypeType
  = MediaTypeType String

derive instance eqMediaTypeType ∷ Eq MediaTypeType

newtype MediaTypeSubtype
  = MediaTypeSubtype String

derive instance eqMediaTypeSubtype ∷ Eq MediaTypeSubtype

-- | Falling back to full `s`... it should never happen :-P
mediaTypeType ∷ MediaType → MediaTypeType
mediaTypeType (MediaType s) = case String.split (String.Pattern "/") s of
  [ mt, st ] → MediaTypeType mt
  otherwise → MediaTypeType s

mediaTypeSubtype ∷ MediaType → MediaTypeSubtype
mediaTypeSubtype (MediaType s) = case String.split (String.Pattern "/") s of
  [ mt, st ] → MediaTypeSubtype st
  otherwise → MediaTypeSubtype s

data MediaPattern
  = ProperMediaType MediaType
  -- | image/*, text/* etc.
  | AnySubtype MediaTypeType
  -- | */*
  | AnyMedia

-- | UnkonwnMediaPattern String
derive instance eqMediaPattern ∷ Eq MediaPattern

printMediaPattern ∷ MediaPattern → String
printMediaPattern AnyMedia = "*/*"

printMediaPattern (AnySubtype (MediaTypeType t)) = t <> "/*"

printMediaPattern (ProperMediaType (MediaType t)) = t

parseMediaPattern ∷ String → MediaPattern
parseMediaPattern "*/*" = AnyMedia

parseMediaPattern p = case String.split (String.Pattern "/") p of
  [ mt, "*" ] → AnySubtype (MediaTypeType mt)
  otherwise → ProperMediaType (MediaType p)

parseHeader ∷ String → Array { pattern ∷ MediaPattern, q ∷ Maybe String }
parseHeader h = do
  v ← String.split (String.Pattern ",") h
  case String.split (String.Pattern ";") v of
    [ m ] → [ { pattern: parseMediaPattern m, q: Nothing } ]
    [ m, q ] → [ { pattern: parseMediaPattern m, q: Just q } ]
    otherwise → []

matches ∷ MediaType → MediaPattern → Boolean
matches mt =
  let
    mtt = mediaTypeType mt
  in
    case _ of
      AnyMedia → true
      AnySubtype mtt' → mtt == mtt'
      ProperMediaType pt → pt == mt

foreign import data ResponseContentTypeStep ∷ Type → Type → TypeExpr → TypeExpr

type ResponseContentType res
  = (FoldrWithIndex ResponseContentTypeStep NilExpr <<< FromRow) (RProxy res)

instance evalResponseContentTypeStepMatch ∷
  Eval (ResponseContentTypeStep (SProxy "ok") (FProxy (OkF contentType)) acc) (SProxy contentType)
else instance evalResponseContentTypeStepNoMatch ∷
  (Eval acc acc') ⇒
  Eval (ResponseContentTypeStep idx t acc) acc'

data ResponseFolding
  = ResponseFolding

instance foldingResponseFolding ∷
  ( Eval (ResponseContentType res) (SProxy contentType)
  , IsSymbol contentType
  , Row.Lacks contentType response
  , Row.Cons contentType (Response.Duplex aff (VariantF res i) (VariantF res o)) response response'
  ) ⇒
  Folding
    ResponseFolding
    { | response }
    (Response.Duplex aff (VariantF res i) (VariantF res o))
    { | response' } where
  folding _ acc r = Record.insert (SProxy ∷ SProxy contentType) r acc

newtype Accept v
  = Accept v

data RequestParserFolding o
  = RequestParserFolding (RequestParser o) MediaPattern

instance foldingRequestParserFolding ∷
  ( IsSymbol contentType
  , Row.Cons contentType req vReq_ vReq
  ) ⇒
  Folding
    (RequestParserFolding req)
    (RequestParser (Variant vReq))
    (SProxy contentType)
    (RequestParser (Variant vReq)) where
  folding (RequestParserFolding prs mp) vprs ct = do
    let
      mt = MediaType (reflectSymbol ct)

      prs' = Variant.inj ct <$> prs
    vprs <|> Control.Lazy.defer \_ → parserBuilder mt prs' mp

parserBuilder ∷ ∀ a. MediaType → RequestParser a → MediaPattern → RequestParser a
parserBuilder mediaType@(MediaType mtStr) prs pattern = do
  let
    match = (mediaType `matches` _)
  if match pattern then
    prs
  else
    Request.Duplex.Parser.Chomp \_ →
      Request.Duplex.Parser.Fail (Request.Duplex.Parser.Expected mtStr $ printMediaPattern pattern)

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

-- | Just provide SProxies with content types so the above `RequestDuplex` folidngs have
-- | simpler signature.
data ResponseContentTypesMapping
  = ResponseContentTypesMapping

-- | We use `Aff` just as an placeholder for polymorphic to
-- | make compiler happy because we actually don't use this type
-- | when we do request folding.
-- | We should probably handle also cases where
instance mappingContentTypesMapping ∷
  ( Eval (ResponseContentType res) (SProxy contentType)
  , TypeEquals f (Response.Duplex Aff (VariantF res i) (VariantF res o))
  , IsSymbol contentType
  ) ⇒
  Mapping
    ResponseContentTypesMapping
    f
    (SProxy contentType) where
  mapping _ _ = SProxy

-- | First goes the value type, second is the label and last is accumulator.
foreign import data HomogeneousRowStep ∷ Type → Type → TypeExpr → TypeExpr

instance evalResultVariant ∷
  (Eval te (RLProxy t)) ⇒
  Eval (HomogeneousRowStep a (SProxy l) te) (RLProxy (RL.Cons l a t))

requestFolding ∷
  ∀ res req cts vReq.
  HMap ResponseContentTypesMapping res cts ⇒
  Eval ((ToRow <<< Foldr' (HomogeneousRowStep req) NilExpr) cts) (RProxy vReq) ⇒
  HFoldl (RequestParserFolding req) (RequestParser (Variant vReq)) cts (RequestParser (Variant vReq)) ⇒
  HFoldl
    (RequestPrinterFolding req)
    (Variant () → RequestPrinter)
    cts
    (Variant vReq → RequestPrinter) ⇒
  RequestDuplex' req →
  res →
  RequestDuplex' (Accept (Variant vReq))
requestFolding (RequestDuplex prt prs) responseDuplexes = do
  let
    lbls ∷ cts
    lbls = hmap ResponseContentTypesMapping responseDuplexes

    prs' = RequestDuplex.Parser.Chomp chomp
      where
      noMatch ∷ RequestParser (Variant vReq)
      noMatch =
        Request.Duplex.Parser.Chomp \_ →
          Request.Duplex.Parser.Fail (RequestDuplex.Parser.Expected "Provide a better error" " for non matching content type against Accept header value")

      buildParser ∷ MediaPattern → RequestParser (Variant vReq) → RequestParser (Variant vReq)
      buildParser mp rest =
        hfoldl
          (RequestParserFolding prs mp)
          rest
          lbls

      chomp state@{ headers } = do
        let
          pts = case parseHeader <$> Tuple.lookup hAccept headers of
            Just [] → [ buildParser AnyMedia ]
            Just arr → map (buildParser <<< _.pattern) arr
            Nothing → [ buildParser AnyMedia ]
        runRequestParser state (ala Profunctor.Join foldMap pts noMatch)

    prt' ∷ Accept (Variant vReq) → RequestPrinter
    prt' (Accept i) = hfoldl (RequestPrinterFolding prt) (Variant.case_ ∷ Variant () → RequestPrinter) lbls i
  RequestDuplex prt' (Accept <$> prs')

-- | TODO: POSSIBLY A BUGGY STUB! We should check for patterns like img/* etc. probably
-- accepts ∷ ∀ eff. MediaPattern → Run (Request + eff) Boolean
-- accepts pattern = do
--   patterns ← accept
--   pure $ pattern `Array.elem` map _.pattern patterns
