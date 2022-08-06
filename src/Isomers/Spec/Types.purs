module Isomers.Spec.Types where


import Data.Newtype (class Newtype)
import Isomers.Request (Accum, Duplex) as Request
import Isomers.Request.Accum (rootDuplex, unifyRoute) as Request.Accum
import Type.Prelude (class TypeEquals)

-- | TODO: Is changing the order and moving `ireq` and `oreq`
-- | to the end worth it? We can provide __probably__ more useful
-- | instances then....
newtype AccumSpec route ireq oreq res = AccumSpec
  { request :: Request.Accum route ireq oreq
  , response :: res
  }

derive instance newtypeAccumSpec :: Newtype (AccumSpec route ireq oreq res) _

unifyRoute
  :: forall ireq oreq res route route'
   . TypeEquals route route'
  => AccumSpec route ireq oreq res
  -> AccumSpec route' ireq oreq res
unifyRoute (AccumSpec { request, response }) = AccumSpec { request: Request.Accum.unifyRoute request, response }

newtype Spec ireq oreq res = Spec
  { request :: Request.Duplex ireq oreq
  , response :: res
  }

type Spec' req res = Spec req req res

derive instance newtypeSpec :: Newtype (Spec ireq oreq res) _

-- | Let's make an initial unit an empty record `{}` for now.
-- | This is consistent with combinators like `hnil`, `scalar` etc.
-- | from `Request.Accum`.
rootAccumSpec :: forall ireq oreq res. AccumSpec {} ireq oreq res -> Spec ireq oreq res
rootAccumSpec (AccumSpec { request: accum, response }) = Spec
  { request: Request.Accum.rootDuplex accum
  , response
  }

