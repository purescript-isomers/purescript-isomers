module Isomers.JSEngine where

import Prelude

import Global.Unsafe (unsafeStringify)
import Unsafe.Reference (unsafeRefEq)

foreign import data JSEngine ∷ Type

-- | We have two cases
foreign import node ∷ JSEngine
foreign import browser ∷ JSEngine

-- | Current engine constant
foreign import jsEngine ∷ JSEngine

instance eqEngine ∷ Eq JSEngine where
  eq = unsafeRefEq

instance showEngine ∷ Show JSEngine where
  show e = "JSEngine (" <> unsafeStringify e <> ")"
