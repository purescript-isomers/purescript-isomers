module Isomer.Contrib.Request.Duplex where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Request.Duplex (RequestDuplex(..), RequestDuplex')
import Request.Duplex.Parser (RequestParser)
import Request.Duplex.Printer (RequestPrinter(..))
import Request.Duplex.Types (RequestState)

unitDuplex ∷ RequestDuplex' Unit
unitDuplex = RequestDuplex (\_ → mempty) (pure unit)


-- duplex ∷
--   ∀ a rnd req req' res.
--   Eval (Tuples a (RProxy req)) (RProxy req') ⇒
--   RequestDuplex' a →
--   Raw req res rnd →
--   Raw req' res rnd
-- duplex (RequestDuplex aprt aprs) (Raw { codecs: codecs@{ request: RequestDuplex vprt vprs }, renderers }) = Raw { renderers, codecs: codecs { request = request' } }
--   where
--   prs' = ado
--     a ← aprs
--     v ← vprs
--     let
--       VariantRep { type: t, value } = unsafeCoerce v
--     in unsafeCoerce (VariantRep { type: t, value: a /\ value })
-- 
--   prt' v =
--     let
--       t ∷ a /\ Variant req
--       t =
--         let
--           VariantRep { type: t, value } = unsafeCoerce v
-- 
--           a /\ b = value
--         in
--           a /\ unsafeCoerce (VariantRep { type: t, value: b })
-- 
--       a /\ v' = t
--     in
--       aprt a <> vprt v'
-- 
--   request' = RequestDuplex prt' prs'

-- data HDuplexMapping prt prs = HDuplexMapping prt prs
-- 
-- instance hmapRequestDuplex ∷
--   ( HMap prsMapping o o'
--   , HFoldl prtMapping i' Unit (i → RequestPrinter)
--   ) ⇒
--   Mapping (HDuplexMapping prtMapping (RequestParser prsMapping)) (RequestDuplex i o) (RequestDuplex i' o') where
-- 
--   mapping (HDuplexMapping prtMapping prsMapping) (RequestDuplex prt prs) =
--     RequestDuplex prt' prs'
--     where
--     prt' i' = do
--       let
--         i /\ rp = hfoldl prtMapping i' unit
--       rp <> prt i
--     prs' = ado
--       prsMapping ← prsMapping
--       v ← prs
--       in hmap prsMapping v
-- 
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
