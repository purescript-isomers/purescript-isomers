module WebRow.Hybrid.Router where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Data.Variant (class Contractable, contract, expand) as Variant
import Data.Variant.Internal (VariantRep(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row (class Cons, class Union) as Row
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, kind RowList)
import Prim.Symbol (class Append) as Symbol
import Record (union) as Record
import Record.Builder (Builder) as Record.Builder
import Record.Prefix (PrefixProps, add) as Record.Prefix
import Routing.Duplex (RouteDuplex', prefix) as Routing.Duplex
import Routing.Duplex (RouteDuplex(..), RouteDuplex')
import Routing.Duplex.Generic.Variant (Updater, modify, update) as Generic.Variant
import Routing.Duplex.Generic.Variant (class VariantParser, class VariantPrinter)
import Routing.Duplex.Generic.Variant (variant) as Routing.Duplex.Generic.Variant
import Routing.Duplex.Parser (RouteError(..), RouteParser(..), RouteResult(..)) as Duplex.Parser
import Routing.Duplex.Parser (RouteParser)
import Routing.Duplex.Printer (RoutePrinter(..))
import Type.Eval (class Eval)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (RProxy)
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Hybrid.Contrib.Type.Eval.Tuple (Tuples)
import WebRow.Hybrid.Data.Variant.Prefix (class PrefixRow, class UnprefixRow, add, remove) as Variant.Prefix

newtype Router response request
  = Router
  { response ∷ { | response }
  , request ∷ RouteDuplex' (Variant request)
  }

duplex ∷
  ∀ a response request request'.
  Eval (Tuples a (RProxy request)) (RProxy request') ⇒
  Routing.Duplex.RouteDuplex' a →
  Router response request →
  Router response request'
duplex (RouteDuplex aprt aprs) (Router { response, request: RouteDuplex vprt vprs }) = Router { response, request: request' }
  where
  prs' = ado
    a ← aprs
    v ← vprs
    let
      VariantRep { type: t, value } = unsafeCoerce v
    in unsafeCoerce (VariantRep { type: t, value: a /\ value })

  prt' v =
    let
      t ∷ a /\ Variant request
      t =
        let
          VariantRep { type: t, value } = unsafeCoerce v

          a /\ b = value
        in
          a /\ unsafeCoerce (VariantRep { type: t, value: b })

      a /\ v' = t
    in
      aprt a <> vprt v'

  request' = RouteDuplex prt' prs'

data PrefixLabels (sep ∷ Symbol)
  = PrefixLabels

instance foldingWithIndexPrefixLabels ::
  ( IsSymbol l
  , Symbol.Append l sep sym
  , IsSymbol sep
  , IsSymbol sym
  , Row.Union response piresponse response'
  , Row.Union piresponse response response'
  , Row.Union request pirequest request'
  , Row.Union pirequest request request'
  , Variant.Contractable request' request
  , Variant.Contractable request' pirequest
  , RowToList irequest irl
  , RowToList pirequest pirl
  , Variant.Prefix.PrefixRow sym irl pirequest
  , Variant.Prefix.UnprefixRow sym pirl irequest
  , HFoldlWithIndex
      (Record.Prefix.PrefixProps sym)
      (Record.Builder.Builder {} {})
      { | iresponse }
      (Record.Builder.Builder {} { | piresponse })
  ) =>
  FoldingWithIndex
    (PrefixLabels sep)
    (SProxy l)
    (Router response request)
    (Router iresponse irequest)
    (Router response' request') where
  foldingWithIndex _ _ (Router r) (Router ir) =
    Router
      { response: (Record.union r.response (Record.Prefix.add prop ir.response)) ∷ { | response' }
      , request:
          RouteDuplex
            ( \i →
                let
                  i' ∷ Maybe (Variant pirequest)
                  i' = Variant.contract i

                  i'' ∷ Maybe (Variant irequest)
                  i'' = Variant.Prefix.remove prop <$> i'
                in
                  fromMaybe
                    (RoutePrinter identity)
                    (prt <$> ((Variant.contract i) ∷ Maybe (Variant request)) <|> iprt <$> i'')
            )
            (Variant.expand <$> prs <|> (Variant.expand <<< ((Variant.Prefix.add prop) ∷ Variant irequest → Variant pirequest)) <$> iprs)
      }
    where
    prop = SProxy ∷ SProxy sym

    RouteDuplex prt prs = r.request

    RouteDuplex iprt iprs = ir.request

class PrefixRoute (rl ∷ RowList) request where
  prefixRoute ∷ RLProxy rl → Generic.Variant.Updater { | request }

instance prefixRouteNil ∷ PrefixRoute RowList.Nil request where
  prefixRoute _ = mempty
else instance prefixRouteEmptyCons ::
  (PrefixRoute tail request, Row.Cons "" (Router response request) r' request) =>
  PrefixRoute (RowList.Cons "" (Router response request) tail) request where
  prefixRoute _ = prefixRoute (RLProxy ∷ RLProxy tail)
else instance prefixRouteCons ::
  (IsSymbol sym, PrefixRoute tail request, Row.Cons sym (Router response rd) r' request) =>
  PrefixRoute (RowList.Cons sym (Router response rd) tail) request where
  prefixRoute _ = Generic.Variant.modify prop step <> prefixRoute (RLProxy ∷ RLProxy tail)
    where
    step (Router { response, request: r }) =
      Router
        { response
        , request: Routing.Duplex.prefix (reflectSymbol prop) r
        }

    prop = SProxy ∷ SProxy sym

prefixLabels ::
  ∀ d i r sep.
  HFoldlWithIndex (PrefixLabels sep) (Router () ()) { | i } (Router d r) =>
  SProxy sep →
  { | i } →
  Router d r
prefixLabels _ =
  hfoldlWithIndex (PrefixLabels ∷ PrefixLabels sep)
    (Router { response: {}, request: RouteDuplex mempty fail } ∷ Router () ())
  where
  fail ∷ RouteParser (Variant ())
  fail = Duplex.Parser.Chomp $ const $ Duplex.Parser.Fail Duplex.Parser.EndOfPath

prefix ∷
  ∀ d i il r sep.
  HFoldlWithIndex (PrefixLabels sep) (Router () ()) { | i } (Router d r) =>
  RowToList i il ⇒
  PrefixRoute il i ⇒
  SProxy sep →
  Record i →
  Router d r
prefix sep request = prefixLabels sep (Generic.Variant.update (prefixRoute (RLProxy ∷ RLProxy il)) request)

prs ∷ ∀ a. RouteDuplex' a → RouteParser a
prs (RouteDuplex _ p) = p

prt ∷ ∀ a. RouteDuplex' a → (a → RoutePrinter)
prt (RouteDuplex p _) = p

type PrefixRouters
  = Boolean

data Fst
  = Fst PrefixRouters

instance fst ∷ (IsSymbol prop) ⇒ MappingWithIndex Fst (SProxy prop) (Tuple (RouteDuplex a a) b) (RouteDuplex a a) where
  mappingWithIndex (Fst prefixRouters) prop =
    if prefixRouters then
      Routing.Duplex.prefix (reflectSymbol prop) <<< fst
    else
      fst

data Snd
  = Snd

instance snd ∷ Mapping Snd (Tuple a b) b where
  mapping _ = snd

-- | Build a `Router` value from a record of endpoints of the shape `duplex /\ Codecs a b`.
router ∷
  ∀ t13 t15 t16 t6 t7.
  RowToList t7 t6 ⇒
  VariantParser t6 t7 t15 ⇒
  VariantPrinter t6 t7 t15 ⇒
  HMapWithIndex Fst t13 (Record t7) ⇒
  HMap Snd t13 (Record t16) ⇒
  PrefixRouters →
  t13 →
  Router t16 t15
router p r =
  let
    request = Routing.Duplex.Generic.Variant.variant (hmapWithIndex (Fst p) r)

    response = hmap Snd r
  in
    Router
      { response
      , request
      }
