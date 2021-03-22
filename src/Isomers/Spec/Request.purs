module Isomers.Spec.Request
  ( PrefixRoutes(..)
  , DuplexFolding(..)
  , WrapData(..)
  , module Data
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe, fromMaybe)
import Data.Variant (Variant, on)
import Data.Variant (class Contractable, contract, expand, inj) as Variant
import Data.Variant.Prefix (NilExpr, PrefixStep, UnprefixStep)
import Data.Variant.Prefix (PrefixCases, UnprefixCases) as Data.Variant.Prefix
import Data.Variant.Prefix (add, remove) as Variant.Prefix
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex)
import Heterogeneous.Mapping (class Mapping)
import Isomers.Contrib.Request.Duplex.Variant (expand) as Duplex.Variant
import Isomers.Spec.Request.Data (_Data, Data)
import Isomers.Spec.Request.Data (_Data, Data(..), DataFolding, DataMapping) as Data
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Union) as Row
import Prim.Symbol (class Append) as Symbol
import Request.Duplex (RequestDuplex(..))
import Request.Duplex (prefix) as Request.Duplex
import Request.Duplex.Printer (RequestPrinter)
import Type.Eval (class Eval)
import Type.Eval.Foldable (FoldrWithIndex)
import Type.Eval.Function (type (<<<))
import Type.Eval.RowList (FromRow, ToRow)
import Type.Prelude (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.Row (RProxy)

type PrefixRoutes
  = Boolean

-- | Fold record of duplexes into a duplex which encodes `Variant`.
-- | We flatten inner `Variants` into a single one.
data DuplexFolding (sep ∷ Symbol)
  = DuplexFolding (SProxy sep) PrefixRoutes

instance duplexFoldingRequestDuplexVariantField ∷
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
    (DuplexFolding sep)
    (SProxy l)
    (RequestDuplex (Variant acc) (Variant acc))
    (RequestDuplex (Variant va) (Variant va))
    (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (DuplexFolding sep prefixRoutes) l acc rd = do
    let
      sym = SProxy ∷ SProxy sym
      RequestDuplex prt prs =
        if prefixRoutes then
          Request.Duplex.prefix (reflectSymbol sym) rd
        else
          rd
      -- | We know what we are doing here...
      RequestDuplex accPrt accPrs = unsafePartial $ Duplex.Variant.expand acc

      prs' = accPrs <|> expand' <$> prs
        where
        expand' ∷ Variant va → Variant acc'
        expand' = Variant.expand <<< pref
          where
          pref ∷ Variant va → Variant va'
          pref = Variant.Prefix.add sym

      prt' ∷ Variant acc' → RequestPrinter
      prt' vacc' =
        let
          va' ∷ Maybe (Variant va')
          va' = Variant.contract vacc'

          va ∷ Maybe (Variant va)
          va = Variant.Prefix.remove sym <$> va'
        in
          fromMaybe (accPrt vacc') (prt <$> va)
    RequestDuplex prt' prs'

else instance duplexFoldingRequestDuplexNonVariantField ∷
  ( Row.Cons l a acc acc'
  , Row.Cons l a () la
  , Row.Union acc la acc'
  , IsSymbol l
  ) ⇒
  FoldingWithIndex
    (DuplexFolding sep)
    (SProxy l)
    (RequestDuplex (Variant acc) (Variant acc))
    (RequestDuplex a a)
    (RequestDuplex (Variant acc') (Variant acc')) where
  foldingWithIndex (DuplexFolding _ prefixRoutes) l (RequestDuplex accPrt accPrs) aRd = do
    let
      RequestDuplex aPrt aPrs =
        if prefixRoutes then
          Request.Duplex.prefix (reflectSymbol l) aRd
        else
          aRd

      accPrt' = accPrt # on l aPrt

      accPrs' = Variant.expand <$> accPrs <|> Variant.inj l <$> aPrs
    RequestDuplex accPrt' accPrs'

data WrapData = WrapData

instance mappingDataNoop ∷ Mapping WrapData (RequestDuplex (Data req) (Data req)) (RequestDuplex (Data req) (Data req)) where
  mapping _ rd = rd
else instance mappingDataWrap ∷ Mapping WrapData (RequestDuplex req req) (RequestDuplex (Data req) (Data req)) where
  mapping _ rd = _Data rd


