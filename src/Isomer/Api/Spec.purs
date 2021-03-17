module Isomer.Api.Spec where

import Prelude
import Control.Alt ((<|>))
import Data.Lens (Iso, Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, on)
import Data.Variant (class Contractable, contract, expand, inj) as Variant
import Data.Variant.Prefix (NilExpr, PrefixStep, UnprefixStep)
import Data.Variant.Prefix (PrefixCases, UnprefixCases) as Data.Variant.Prefix
import Data.Variant.Prefix (add, remove) as Variant.Prefix
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap)
import Isomer.Contrib.Heterogeneous (hmap')
import Isomer.Contrib.Heterogeneous.Mappings (Compose(..)) as Mappings
import Isomer.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Newtype
import Isomer.Contrib.Heterogeneous.Mappings.Record (Get(..)) as Mappings.Record
import Isomer.HTTP (Method(..)) as Isomer.HTTP
import Isomer.HTTP.Method (Method) as HTTP
import Isomer.HTTP.Request (Data) as Request
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList)
import Prim.Symbol (class Append) as Symbol
import Record (insert, union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (prefix) as Request.Duplex
import Request.Duplex.Generic.Variant (class MethodPrefixRoutes, class VariantParser, class VariantPrinter, methodVariant) as Request.Duplex.Generic.Variant
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Request.Duplex.Parser
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter(..))
import Type.Eval (class Eval)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.Row (RProxy)

newtype ResponseCodecs res
  = ResponseCodecs res

derive instance newtypeResponseCodecs ∷ Newtype (ResponseCodecs res) _

newtype Spec request response
  = Spec
  { request ∷ RequestDuplex' request
  , response ∷ ResponseCodecs response
  }

derive instance newtypeSpec ∷ Newtype (Spec req res) _

-- | We use `Data` wrapper to simplify upcomming transformations.
endpoint ∷ ∀ t38 t40. RequestDuplex' t38 → t40 → Spec (Request.Data t38) t40
endpoint request response = Spec { request: request', response: ResponseCodecs response }
  where
  -- _Newtype ∷ ∀ t a s b. Newtype t a ⇒ Newtype s b ⇒ Iso t s a b
  _Data ∷ ∀ a. Iso (Request.Data a) (Request.Data a) a a
  _Data = _Newtype

  request' = _Data request

_request = SProxy ∷ SProxy "request"

_response = SProxy ∷ SProxy "response"

_RequestMapping ∷ Mappings.Compose (Mappings.Record.Get "request") Mappings.Newtype.Unwrap
_RequestMapping = Mappings.Record.Get _request `Mappings.Compose` Mappings.Newtype.Unwrap

_ResponseMapping ∷ Mappings.Compose Mappings.Newtype.Unwrap (Mappings.Compose (Mappings.Record.Get "response") Mappings.Newtype.Unwrap)
_ResponseMapping = Mappings.Newtype.Unwrap `Mappings.Compose` (Mappings.Record.Get _response `Mappings.Compose` Mappings.Newtype.Unwrap)

method ∷
  ∀ t221 t227 t235 t238 t239.
  HMap (Mappings.Compose Mappings.Newtype.Unwrap (Mappings.Compose (Mappings.Record.Get "response") Mappings.Newtype.Unwrap)) t227 { | t221 } ⇒
  HMap (Mappings.Compose (Mappings.Record.Get "request") Mappings.Newtype.Unwrap) t227 { | t239 } ⇒
  RowToList t239 t238 ⇒
  Request.Duplex.Generic.Variant.VariantParser t238 t239 t235 ⇒
  Request.Duplex.Generic.Variant.VariantPrinter t238 t239 t235 ⇒
  Request.Duplex.Generic.Variant.MethodPrefixRoutes t238 t239 ⇒
  t227 →
  Spec (Isomer.HTTP.Method (Variant t235)) (Isomer.HTTP.Method { | t221 })
method r = Spec { request, response: ResponseCodecs response }
  where
  -- | Drop `Spec` from the values
  requests = hmap' _RequestMapping r

  _Method ∷ ∀ a. Iso' (HTTP.Method a) a
  _Method = _Newtype

  request = _Method (Request.Duplex.Generic.Variant.methodVariant requests)

  response = Isomer.HTTP.Method (hmap' _ResponseMapping r)

type PrefixRoutes
  = Boolean

data SpecFolding (sep ∷ Symbol)
  = SpecFolding PrefixRoutes

prefix ∷ ∀ t173 t174. HFoldlWithIndex (SpecFolding ".") (Spec (Variant ()) (Record ())) t173 t174 ⇒ t173 → t174
prefix raw = hfoldlWithIndex (SpecFolding true ∷ SpecFolding ".") emptyVariantSpec raw

emptyVariantSpec ∷ Spec (Variant ()) (Record ())
emptyVariantSpec = Spec { request: emptyVariantDuplex, response: ResponseCodecs {} }

emptyVariantDuplex ∷ RequestDuplex' (Variant ())
emptyVariantDuplex = RequestDuplex mempty fail
  where
  fail ∷ RequestParser (Variant ())
  fail = Request.Duplex.Parser.Chomp $ const $ Request.Duplex.Parser.Fail Request.Duplex.Parser.EndOfPath

-- | We recurse into the records when we encounter them as field value.
-- | Finally this result should be wrapped into the `RecordCodecs` constructor.
-- | This case is buggy
instance prefixFoldingRec ∷
  ( HFoldlWithIndex (SpecFolding sep) (Spec (Variant ()) (Record ())) { | r } r'
  , FoldingWithIndex (SpecFolding sep) l acc r' (Spec req res)
  ) ⇒
  FoldingWithIndex (SpecFolding sep) l acc { | r } (Spec req res) where
  foldingWithIndex pref l acc r = do
    let
      r' = hfoldlWithIndex pref emptyVariantSpec r
    foldingWithIndex pref l acc r'

-- | We split this folding into separate foldings over request and response codecs rows.
instance prefixFoldingSpec ∷
  ( FoldingWithIndex (SpecFolding sep) (SProxy l) (ResponseCodecs resAcc) (ResponseCodecs res) (ResponseCodecs res')
  , FoldingWithIndex (SpecFolding sep) (SProxy l) (RequestDuplex' reqAcc) (RequestDuplex' req) (RequestDuplex' req')
  ) ⇒
  FoldingWithIndex
    (SpecFolding sep)
    (SProxy l)
    (Spec reqAcc resAcc)
    (Spec req res)
    (Spec req' res') where
  foldingWithIndex pref l (Spec acc) (Spec { request, response }) = do
    let
      request' = foldingWithIndex pref l acc.request request

      response' = foldingWithIndex pref l acc.response response
    Spec { request: request', response: response' }

-- | Given a record field with `ResponseCodecs` which contain record of codecs we build
-- | a single record which contains codecs.
instance prefixFoldingResponseCodecsRecord ∷
  ( HFoldlWithIndex (Record.Prefix.PrefixProps sym) (Record.Builder.Builder {} {}) { | res } (Record.Builder.Builder {} { | res' })
  , Symbol.Append l sep sym
  , Row.Union res' acc acc'
  ) ⇒
  FoldingWithIndex (SpecFolding sep) (SProxy l) (ResponseCodecs { | acc }) (ResponseCodecs { | res }) (ResponseCodecs { | acc' }) where
  foldingWithIndex pref l (ResponseCodecs acc) (ResponseCodecs v) = do
    let
      sym = SProxy ∷ SProxy sym
    ResponseCodecs (Record.union (Record.Prefix.add sym v) acc)
-- | Given a record field with `ResponseCodecs` which contain a plain codec value
-- | we just append this codec to the resulting record.
else instance prefixFoldingResponseCodecsPlain ∷
  ( IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l a acc acc'
  ) ⇒
  FoldingWithIndex
    (SpecFolding sep)
    (SProxy l)
    (ResponseCodecs { | acc })
    (ResponseCodecs a)
    (ResponseCodecs { | acc' }) where
  foldingWithIndex pref l (ResponseCodecs acc) (ResponseCodecs v) = ResponseCodecs $ Record.insert l v acc

-- | This instance is hard to follow but it "just" prefixes the resulting duplex `Variant`.
-- | It seems that even though there is so much of the type level magic we are not able
-- | to write fully typesafe printer (we use `fromMaybe` there).
instance prefixFoldingRequestDuplexVariantField ∷
  ( HFoldlWithIndex (Data.Variant.Prefix.PrefixCases sym va') Unit (Variant va) (Variant va')
  , HFoldlWithIndex (Data.Variant.Prefix.UnprefixCases sym va) Unit (Variant va') (Variant va)
  , Eval ((ToRow <<< FoldrWithIndex (PrefixStep sym) NilExpr <<< FromRow) (RProxy va)) (RProxy va')
  , Eval ((ToRow <<< FoldrWithIndex (UnprefixStep sym) NilExpr <<< FromRow) (RProxy va')) (RProxy va)
  , Variant.Contractable acc' acc
  , Variant.Contractable acc' va'
  , Symbol.Append l sep sym
  , Row.Union va' acc acc'
  , Row.Union acc va' acc'
  , IsSymbol l
  , IsSymbol sym
  ) ⇒
  FoldingWithIndex
    (SpecFolding sep)
    (SProxy l)
    (RequestDuplex (Variant acc) (Variant acc))
    (RequestDuplex (Variant va) (Variant va))
    (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (SpecFolding prefixRoutes) l (RequestDuplex accPrt accPrs) aRd = do
    let
      sym = SProxy ∷ SProxy sym

      RequestDuplex aPrt aPrs =
        if prefixRoutes then
          Request.Duplex.prefix (reflectSymbol sym) aRd
        else
          aRd

      accPrs' = Variant.expand <$> accPrs <|> expand' <$> aPrs
        where
        expand' ∷ Variant va → Variant acc'
        expand' = Variant.expand <<< pref
          where
          pref ∷ Variant va → Variant va'
          pref = Variant.Prefix.add sym

      accPrt' ∷ Variant acc' → RequestPrinter
      accPrt' vacc' =
        let
          va' ∷ Maybe (Variant va')
          va' = Variant.contract vacc'

          va ∷ Maybe (Variant va)
          va = Variant.Prefix.remove sym <$> va'

          vAcc ∷ Maybe (Variant acc)
          vAcc = Variant.contract vacc'
        in
          fromMaybe
            (RequestPrinter identity)
            (aPrt <$> va <|> accPrt <$> vAcc)
    RequestDuplex accPrt' accPrs'
else instance prefixFoldingRequestDuplexNonVariantField ∷
  ( Row.Cons l a acc acc'
  , Row.Cons l a () la
  , Row.Union acc la acc'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex
    (SpecFolding sep)
    (SProxy l)
    (RequestDuplex (Variant acc) (Variant acc))
    (RequestDuplex a a)
    (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (SpecFolding prefixRoutes) l (RequestDuplex accPrt accPrs) aRd = do
    let
      RequestDuplex aPrt aPrs =
        if prefixRoutes then
          Request.Duplex.prefix (reflectSymbol l) aRd
        else
          aRd

      accPrt' = accPrt # on l aPrt

      accPrs' = Variant.expand <$> accPrs <|> Variant.inj l <$> aPrs
    RequestDuplex accPrt' accPrs'

instance hfoldlWithIndexSpec ∷
  HFoldlWithIndex (SpecFolding sep) acc (Spec request response) (Spec request response) where
  hfoldlWithIndex _ _ r = r
