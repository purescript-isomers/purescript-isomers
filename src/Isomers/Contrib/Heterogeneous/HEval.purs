module Isomers.Contrib.Heterogeneous.HEval where

import Prelude

import Data.Profunctor.Strong ((&&&), (***)) as Strong
import Data.Profunctor.Strong (first, second)
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class Folding, class HFoldl, folding, hfoldl)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap, mapping)
import Isomers.Contrib.Heterogeneous.Filtering (class HFilter, hfilter)
import Prelude ((<<<)) as Prelude
import Prim.Boolean (False, True, kind Boolean)
import Type.Prelude (BProxy)

data HCompose f g
  = HCompose f g

infixr 9 type HCompose as <<<
infixr 9 HCompose as <<<

instance mappingCompose ∷ (Mapping g a b, Mapping f b c) ⇒ Mapping (HCompose f g) a c where
  mapping (HCompose f g) = mapping f Prelude.<<< mapping g

instance foldingCompose ∷ (Folding g gacc a b, Folding f facc b c) ⇒ Folding (HCompose (f /\ facc) (g /\ gacc)) Unit a c where
  folding (HCompose (f /\ facc) (g /\ gacc)) _ = folding f facc Prelude.<<< folding g gacc

class HEval i o | i → o where
  heval ∷ i → o

instance hevalCompose ∷ (HEval f (b → c), HEval g (a → b)) ⇒ HEval (HCompose f g) (a → c) where
  heval (HCompose f g) = heval f Prelude.<<< heval g

data DoHMap f = DoHMap f

instance hevalDoHMap ∷ (HMap f i o) ⇒ HEval (DoHMap f) (i → o) where
  heval (DoHMap f) = hmap f

data DoHFoldl f acc = DoHFoldl f acc

instance hevalDoHFoldl ∷ (HFoldl f acc a b) ⇒ HEval (DoHFoldl f acc) (a → b) where
  heval (DoHFoldl f acc) = hfoldl f acc

data DoHFilter f = DoHFilter f

instance hevalDoHFilter ∷ (HFilter f a b) ⇒ HEval (DoHFilter f) (a → b) where
  heval (DoHFilter f) = hfilter f

data DoIdentity = DoIdentity

instance hevalDoIdentity ∷ HEval DoIdentity (i → i) where
  heval DoIdentity = identity

data DoConst c = DoConst c

instance hevalDoConst ∷ HEval (DoConst c) (i → c) where
  heval (DoConst c) _ = c

data DoApply f = DoApply f

instance hevalDoApply ∷ HEval (DoApply (i → j)) (i → j) where
  heval (DoApply f) i = f i

data DoFanout f g = DoFanout f g

infixr 3 type DoFanout as &&&
infixr 3 DoFanout as &&&

instance hevalDoFanout ∷ (HEval f (i → o), HEval g (i → p)) ⇒ HEval (DoFanout f g) (i → (o /\ p)) where
  heval (DoFanout f g) = heval f Strong.&&& heval g

data DoSplit f g = DoSplit f g

infixr 3 type DoSplit as ***
infixr 3 DoSplit as ***

instance hevalDoSplit ∷ (HEval f (i → o), HEval g (j → p)) ⇒ HEval (DoSplit f g) (i /\ j → o /\ p) where
  heval (DoSplit f g) = heval f Strong.*** heval g

data DoFirst f = DoFirst f

instance hevalDoFirst ∷ (HEval f (i → i')) ⇒ HEval (DoFirst f) (i /\ j → i' /\ j) where
  heval (DoFirst f) = first $ heval f

data DoSecond f = DoSecond f

instance hevalDoSecond ∷ (HEval f (j → j')) ⇒ HEval (DoSecond f) (i /\ j → i /\ j') where
  heval (DoSecond f) = second $ heval f

class HIfThenElse (b ∷ Boolean) t f r | b t f → r where
  hifThenElse ∷ BProxy b → t → f → r

instance hifThenElseTrue ∷ HIfThenElse True t f t where
  hifThenElse _ t _ = t

instance hifFalse ∷ HIfThenElse False t f f where
  hifThenElse _ _ f = f

data DoHIfThenElse c t f = DoHIfThenElse c t f

instance heavalDoHIfThenElse ∷
  ( HEval c (i → BProxy b)
  , HEval t (i → t')
  , HEval f (i → f')
  , HIfThenElse b t' f' r
  ) ⇒ HEval (DoHIfThenElse c t f) (i → r) where
  heval (DoHIfThenElse c t f) i = do
    let
      b = heval c i
      t' = heval t i
      f' = heval f i
    hifThenElse b t' f'


