module Isomers.Contrib.Request.Duplex.Heterogenous where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, hmap)
import Request.Duplex (RequestDuplex(..))
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter)

mapping ∷ ∀ i i' o o' prsMapping prtFolding.
  HMap prsMapping o o' ⇒
  HFoldl prtFolding Unit i' (i /\ RequestPrinter) ⇒
  prtFolding →
  (RequestParser prsMapping) →
  RequestDuplex i o →
  RequestDuplex i' o'
mapping prtFolding prsMapping (RequestDuplex prt prs) = RequestDuplex prt' prs'
  where
  prt' i' = do
    let
      i /\ rp = hfoldl prtFolding unit i'
    rp <> prt i
  prs' = ado
    prsMapping ← prsMapping
    v ← prs
    in hmap prsMapping v


-- newtype TupleCons m = TupleCons m
-- 
-- -- instance tupleMappingPrinter ∷ Folding (TuplePrefix (a → RequestPrinter)) (a /\ i → RequestPrinter) (i → RequestPrinter) where
-- --   mapping (TupleMapping aprt) iprt (a /\ i) = aprt a <> iprt i
-- -- 
-- 
-- -- instance hmapRequestDuplex ∷
-- --   ( HMap prsMapping (RequestParser o) (RequestParser o')
-- --   , HMap prtMapping (i → RequestPrinter) (i' → RequestPrinter)
-- --   ) ⇒
-- --   Mapping (HDuplexMapping prtMapping prsMapping) (RequestDuplex i o) (RequestDuplex i' o') where
-- -- 
-- --   mapping (HDuplexMapping prtMapping prsMapping) (RequestDuplex prt prs) =
-- --     RequestDuplex (hmap prtMapping prt) (hmap prsMapping prs)
-- 
-- 
-- 
