module Isomers.HTTP.Response.Node where

import Prelude

import Effect.Aff (Aff)
import Node.Encoding (Encoding)
import Type.Row (type (+))

type Headers r
  = ( setHeader ∷ String → String → Aff Unit -- Effect Unit
    , setHeaders ∷ String → Array String → Aff Unit -- Effect Unit
    | r
    )

type Status r
  = ( setStatusCode ∷ Int → Aff Unit
    , setStatusMessage ∷ String → Aff Unit -- Effect Unit
    | r
    )

-- | We provide an API for body writing here directly
-- | instead of the original:
-- | ( asStream ∷ Node.Stream.Writable () )
type Body r
  = ( body ∷
        { writeString ∷ Encoding → String → Aff Unit }
    | r
    )

-- | TODO: Make this a record again
-- | so we can use it in a more polymorphic manner.
-- | Some scenarios doesn't care about streaming
-- | and can be easier to test etc.
newtype Interface = Interface { | Headers + Status + Body + () }
