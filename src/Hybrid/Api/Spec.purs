module Hybrid.Api.Spec where

import Prelude

import Control.Alt ((<|>))
import Data.Lens (Iso, Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Profunctor (class Profunctor)
import Data.Variant (Variant, on)
import Data.Variant (class Contractable, contract, expand, inj) as Variant
import Data.Variant.Prefix (NilExpr, PrefixStep, UnprefixCases(..), UnprefixStep)
import Data.Variant.Prefix (PrefixCases(..), UnprefixCases(..)) as Data.Variant.Prefix
import Data.Variant.Prefix (add, remove) as Variant.Prefix
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, folding, foldingWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, hmap)
import Hybrid.Contrib.Heterogeneous (class HMap', hmap')
import Hybrid.Contrib.Heterogeneous.Mappings (Compose(..)) as Mappings
import Hybrid.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..)) as Mappings.Mappings.Newtype
import Hybrid.Contrib.Heterogeneous.Mappings.Newtype (Unwrap(..), Wrap(..)) as Mappings.Newtype
import Hybrid.Contrib.Heterogeneous.Mappings.Record (Delete, Get(..), Insert) as Mappings.Record
import Hybrid.Contrid.Heterogeneous.Mappings.Tuple (Cons, Fst, Snd) as Mappings.Tuple
import Hybrid.HTTP.Request (Data(..), Method(..)) as Request
import Hybrid.HTTP.Request.Data (DataFolding, DataMapping)
import Hybrid.HTTP.Request.Duplex (dataRecordInsert, dataTupleCons) as Hybrid.HTTP.Request.Duplex
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList (class RowToList)
import Prim.Symbol (class Append) as Symbol
import Record (insert, merge, set, union) as Record
import Record.Builder (Builder) as Record.Buidler
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex (prefix) as Request.Duplex
import Request.Duplex.Generic.Variant (class MethodPrefixRoutes, class VariantParser, class VariantPrinter, methodVariant) as Request.Duplex.Generic.Variant
import Request.Duplex.Parser (RequestParser(..), RouteError(..), RouteResult(..)) as Request.Duplex.Parser
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter(..))
import Routing.Duplex.Printer (RoutePrinter(..))
import Type.Eval (class Eval)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (RProxy(..), SProxy, reflectSymbol)
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row (RProxy(..))

-- type Codecs request response
--   = { request ∷ RequestDuplex' (Variant request)
--     , response ∷ response
--     }
-- 
data ResponseCodec i o
  = ResponseCodec
    (i → String)
    (String → Maybe o)

type ResponseCodec' a
  = ResponseCodec a a

derive instance functorCodec ∷ Functor (ResponseCodec i)

instance profunctorCodec ∷ Profunctor ResponseCodec where
  dimap f g (ResponseCodec encode decode) = ResponseCodec (encode <<< f) (map g <<< decode)

newtype ResponseCodecs res = ResponseCodecs res

derive instance newtypeResponseCodecs ∷ Newtype (ResponseCodecs res) _

newtype MethodsResponses r = MethodsResponses { | r }

newtype Raw request response
  = Raw
  { request ∷ RequestDuplex' request
  , response ∷ ResponseCodecs response
  }

derive instance newtypeRaw ∷ Newtype (Raw req res) _

-- | We use `Data` wrapper to simplify upcomming transformations
-- endpoint ∷ ∀ req res. RequestDuplex' req → ResponseCodec' res → Raw (Request.Data req) (ResponseCodec' res)
endpoint :: forall t38 t40. RequestDuplex' t38 -> t40 -> Raw (Request.Data t38) t40
endpoint request response = Raw { request: request', response: ResponseCodecs response }
  where
  -- _Newtype ∷ ∀ t a s b. Newtype t a ⇒ Newtype s b ⇒ Iso t s a b
  _Data ∷ ∀ a. Iso (Request.Data a) (Request.Data a) a a
  _Data = _Newtype

  request' = _Data request


_request = SProxy ∷ SProxy "request"

_response = SProxy ∷ SProxy "response"

_RequestMapping = Mappings.Record.Get _request `Mappings.Compose` Mappings.Newtype.Unwrap

_ResponseMapping = Mappings.Newtype.Unwrap `Mappings.Compose` Mappings.Record.Get _response `Mappings.Compose` Mappings.Newtype.Unwrap

-- method ∷
--   ∀ t60 t63 t64 t65 t66.
--   HMap' (Mappings.Compose (Mappings.Record.Get "response") Mappings.Newtype.Unwrap) t60 t66 ⇒
--   HMap (Mappings.Compose (Mappings.Record.Get "request") Mappings.Newtype.Unwrap) t60 (Record t65) ⇒
--   RowToList t65 t64 ⇒
--   Request.Duplex.Generic.Variant.VariantParser t64 t65 t63 ⇒
--   Request.Duplex.Generic.Variant.VariantPrinter t64 t65 t63 ⇒
--   Request.Duplex.Generic.Variant.MethodPrefixRoutes t64 t65 ⇒
--   t60 →
--   Raw (Variant t63) t66
method r = Raw { request, response: ResponseCodecs response }
  where
  -- | Drop `Raw` from the values
  requests = hmap' _RequestMapping r

  _Method ∷ ∀ m. Iso' (Request.Method m) (Variant m)
  _Method = _Newtype

  request = _Method (Request.Duplex.Generic.Variant.methodVariant requests)

  response = MethodsResponses (hmap' _ResponseMapping r)


type PrefixRoutes = Boolean
data PrefixFolding (sep ∷ Symbol) = PrefixFolding PrefixRoutes

prefix :: forall t173 t174. HFoldlWithIndex (PrefixFolding ".") (Raw (Variant ()) (Record ())) t173 t174 => t173 -> t174
prefix raw = hfoldlWithIndex (PrefixFolding true ∷ PrefixFolding ".") emptyVariantRaw raw

emptyVariantRaw = Raw {request: emptyVariantDuplex, response: ResponseCodecs {}}

emptyVariantDuplex ∷ RequestDuplex' (Variant ())
emptyVariantDuplex = RequestDuplex mempty fail
  where
  fail ∷ RequestParser (Variant ())
  fail = Request.Duplex.Parser.Chomp $ const $ Request.Duplex.Parser.Fail Request.Duplex.Parser.EndOfPath

-- | We recurse into the records when we encounter them as field value.
-- | Finally this result should be wrapped into the `RecordCodecs` constructor.
-- | This case is buggy
instance prefixFoldingRec ∷
  ( HFoldlWithIndex (PrefixFolding sep) (Raw (Variant ()) (Record ())) { | r } r'
  , FoldingWithIndex (PrefixFolding sep) l acc r' r''
  ) ⇒ FoldingWithIndex (PrefixFolding sep) l acc { | r } r'' where
  foldingWithIndex pref l acc r = do
    let
      r' = hfoldlWithIndex pref emptyVariantRaw r
    foldingWithIndex pref l acc r'

-- | We split this folding into separate foldings over request and response codecs rows.
instance prefixFoldingRaw ∷
  ( FoldingWithIndex (PrefixFolding sep) (SProxy l) resAcc (ResponseCodecs res) { | res' }
  , FoldingWithIndex (PrefixFolding sep) (SProxy l) (RequestDuplex' reqAcc) (RequestDuplex' req) (RequestDuplex' req')
  ) ⇒
  FoldingWithIndex (PrefixFolding sep) (SProxy l) (Raw reqAcc resAcc) (Raw req res) (Raw req' { | res'}) where
  foldingWithIndex pref l (Raw acc) (Raw { request, response }) = do
    let
      request' = foldingWithIndex pref l acc.request request
      response' = foldingWithIndex pref l (un ResponseCodecs acc.response) response
    Raw { request: request', response: ResponseCodecs response' }


-- | Given a record field with `ResponseCodecs` which contain record of codecs we build
-- | a single record which contains codecs.
-- | Finally this result should be wrapped into the `RecordCodecs` constructor.
instance prefixFoldingResponseCodecsRecord ∷
  ( HFoldlWithIndex (Record.Prefix.PrefixProps sym) (Record.Builder.Builder {} {}) { | res } (Record.Builder.Builder {} { | res' })
  , Symbol.Append l sep sym
  , Row.Union res' acc acc'
  ) ⇒ FoldingWithIndex (PrefixFolding sep) (SProxy l) { | acc } (ResponseCodecs { | res }) { | acc' } where
  foldingWithIndex pref l acc (ResponseCodecs v) = do
    let
      sym = SProxy ∷ SProxy sym
    Record.union (Record.Prefix.add sym v) acc


-- | Given a record field with `ResponseCodecs` which contain a plain codec value
-- | we just append this codec to the resulting record.
else instance prefixFoldingResponseCodecsPlain ∷
  ( IsSymbol l
  , Row.Lacks l acc
  , Row.Cons l a acc acc'
  ) ⇒ FoldingWithIndex (PrefixFolding sep) (SProxy l) { | acc } (ResponseCodecs a) { | acc' } where
  foldingWithIndex pref l acc (ResponseCodecs v) = Record.insert l v acc

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
  )
  ⇒ FoldingWithIndex (PrefixFolding sep) (SProxy l) (RequestDuplex (Variant acc) (Variant acc)) (RequestDuplex (Variant va) (Variant va)) (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (PrefixFolding prefixRoutes) l (RequestDuplex accPrt accPrs) aRd = do
    let
      sym = SProxy ∷ SProxy sym

      RequestDuplex aPrt aPrs = if prefixRoutes
        then Request.Duplex.prefix (reflectSymbol sym) aRd
        else aRd

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
  )
  ⇒ FoldingWithIndex (PrefixFolding sep) (SProxy l) (RequestDuplex (Variant acc) (Variant acc)) (RequestDuplex a a) (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (PrefixFolding prefixRoutes) l (RequestDuplex accPrt accPrs) aRd = do
    let
      RequestDuplex aPrt aPrs = if prefixRoutes
        then Request.Duplex.prefix (reflectSymbol l) aRd
        else aRd
      accPrt' = accPrt # on l aPrt
      accPrs' = Variant.expand <$> accPrs <|> Variant.inj l <$> aPrs
    RequestDuplex accPrt' accPrs'



-- prefix ∷
--   ∀ i il req res rnd sep.
--   HFoldlWithIndex (PrefixFolding sep) (Raw () () ()) { | i } (Raw req res rnd) =>
--   RowToList i il ⇒
--   PrefixPath il i ⇒
--   SProxy sep →
--   { | i } →
--   Raw req res rnd
