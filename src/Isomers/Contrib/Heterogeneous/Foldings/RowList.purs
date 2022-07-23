module Isomers.Contrib.Heterogeneous.Foldings.RowList where

import Prim.Boolean (False, True)
import Prim.RowList (Nil) as RL
import Prim.RowList (RowList)

class Null (rl :: RowList Type) (b :: Boolean) | rl -> b

instance nullNil :: Null RL.Nil True
else instance nullCons :: Null l False

