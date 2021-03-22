module Isomers.Request.Duplex.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, on)
import Data.Variant (expand, inj) as Variant
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Isomers.Request.Duplex.Duplex (Duplex(..))
import Isomers.Request.Duplex.Variant (empty) as Request.Duplex.Variant
import Prim.Row (class Cons, class Union) as Row
import Request.Duplex.Parser (prefix) as Parser
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter)
import Request.Duplex.Printer (prefix) as Printer
import Type.Prelude (SProxy, reflectSymbol)

type PrefixRoutes = Boolean

newtype VariantStep = VariantStep PrefixRoutes

instance foldingVariantStepRequestParser ∷
  ( Row.Cons l a () la
  , Row.Union v la v'
  , Row.Cons l a v v'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex VariantStep (SProxy l) (RequestParser (r → Variant v)) (RequestParser (r → a)) (RequestParser (r → Variant v')) where
  foldingWithIndex (VariantStep prefixRoutes) l acc prs = do
    let
      prs' = prs # if prefixRoutes
        then
          Parser.prefix (reflectSymbol l)
        else
          identity
    map (Variant.inj l) <$> prs' <|> map Variant.expand <$> acc

instance foldingVariantStepRequestPrinter ∷
  ( Row.Cons l a () la
  , Row.Union v la v'
  , Row.Cons l a v v'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex VariantStep (SProxy l) (Variant v → RequestPrinter) (a → RequestPrinter) (Variant v' → RequestPrinter) where
  foldingWithIndex (VariantStep prefixRoutes) l acc prt = do
    let
      prt' = prt # if prefixRoutes
        then
          compose (Printer.prefix (reflectSymbol l))
        else
          identity
    acc # on l prt'

instance foldingVariantStepDuplex ∷
  ( FoldingWithIndex VariantStep (SProxy l) (RequestParser (r → Variant vo)) (RequestParser (r → o)) (RequestParser (r → Variant vo'))
  , FoldingWithIndex VariantStep (SProxy l) (Variant vi → RequestPrinter) (i → RequestPrinter) (Variant vi' → RequestPrinter)
  ) ⇒
  FoldingWithIndex VariantStep (SProxy l) (Duplex r (Variant vi) (Variant vo)) (Duplex r i o) (Duplex r (Variant vi') (Variant vo')) where
  foldingWithIndex vp l (Duplex vPrt vPrs) (Duplex prt prs) = do
    let
      prt' = foldingWithIndex vp l vPrt prt
      prs' = foldingWithIndex vp l vPrs prs
    Duplex prt' prs'

variant ∷ ∀ i o r rec.
  HFoldlWithIndex VariantStep (Duplex r (Variant ()) (Variant ())) rec (Duplex r (Variant i) (Variant o)) ⇒
  PrefixRoutes →
  rec →
  Duplex r (Variant i) (Variant o)
variant p rec = do
  hfoldlWithIndex (VariantStep p) (Request.Duplex.Variant.empty ∷ Duplex r _ _) rec

