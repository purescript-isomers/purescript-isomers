module Isomers.HTTP.Response.Node where

import Prelude
import Node.Encoding (Encoding)
import Type.Row (type (+))

type Headers aff r
  = ( setHeader ∷ String → String → aff Unit -- Effect Unit
    , setHeaders ∷ String → Array String → aff Unit -- Effect Unit
    | r
    )

type Status aff r
  = ( setStatusCode ∷ Int → aff Unit
    , setStatusMessage ∷ String → aff Unit -- Effect Unit
    | r
    )

-- | We provide an API for body writing here directly
-- | instead of the original:
-- | ( asStream ∷ Node.Stream.Writable () )
type Body aff r
  = ( body ∷
        { writeString ∷ Encoding → String → aff Unit }
    | r
    )

-- | TODO: Make this a record again
-- | so we can use it in a more polymorphic manner.
-- | Some scenarios doesn't care about streaming
-- | and can be easier to test etc.
newtype Interface aff = Interface { | Headers aff + Status aff + Body aff + () }
