module Isomers.Web.Builder.HEval where

import Heterogeneous.Mapping (class Mapping)
import Isomers.Contrib.Heterogeneous.Foldings.RowList (class Null) as Contrib.RowList
import Isomers.Contrib.Heterogeneous.HEval (class HEval)
import Isomers.Contrib.Heterogeneous.HMaybe (HJust(..), HNothing)
import Prim.Boolean (False, True, kind Boolean)
import Prim.RowList (class RowToList)
import Type.Prelude (BProxy(..))

data DoNull = DoNull

instance hevalDoNull ∷ (RowToList r rl, Contrib.RowList.Null rl b) ⇒ HEval DoNull ({ | r } → BProxy b) where
  heval _ _ = BProxy ∷ BProxy b

class IsHJust m (b ∷ Boolean) | m → b

instance isHJust ∷ IsHJust (HJust a) True
else instance isHJustNothing ∷ IsHJust HNothing False

data DoIsHJust = DoIsHJust

instance hevalDoIsHJust ∷ IsHJust i b ⇒ HEval DoIsHJust (i → BProxy b) where
  heval _ _ = BProxy ∷ BProxy b

data FromHJust = FromHJust

instance mappingFromHJust ∷ Mapping FromHJust (HJust a) a where
  mapping _ (HJust a) = a
