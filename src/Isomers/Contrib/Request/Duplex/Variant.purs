module Isomers.Contrib.Request.Duplex.Variant where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant (class Contractable, contract, expand) as Variant
import Data.Variant.Internal (class VariantTags, variantTags)
import Isomers.Contrib.Data.Variant (tag)
import Prim.Row (class Union) as Row
import Prim.RowList (class RowToList)
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex.Parser (as) as RequestDuplex.Parser
import Type.Prelude (RLProxy(..))

-- | Creates a "partial" `Variant` duplex. It won't print additional cases! They should be handled by other duplexes in `alt` chain.
expand ∷
  ∀ v v_ v'.
  Partial ⇒
  Variant.Contractable v' v ⇒
  Row.Union v v_ v' ⇒
  RequestDuplex' (Variant v) →
  RequestDuplex' (Variant v')
expand (RequestDuplex prt prs) = RequestDuplex prt' prs'
  where
  prs' = Variant.expand <$> prs

  prt' v = case Variant.contract v of
    Just v' → prt v'
    Nothing → mempty

contract ∷
  ∀ v v_ v' vl.
  VariantTags vl ⇒
  RowToList v' vl ⇒
  Variant.Contractable v' v ⇒
  Row.Union v v_ v' ⇒
  RequestDuplex' (Variant v') →
  RequestDuplex' (Variant v)
contract (RequestDuplex prt prs) = RequestDuplex prt' prs'
  where
  prs' =
    prs
      # RequestDuplex.Parser.as tag \v' → case Variant.contract v' of
          Just v → Right v
          Nothing → do
            let
              tags = variantTags (RLProxy ∷ RLProxy vl)
            Left $ show tags

  prt' = prt <<< Variant.expand
