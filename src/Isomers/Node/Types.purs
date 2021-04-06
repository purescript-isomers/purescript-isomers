module Isomers.Node.Types where

import Isomers.Node.Request.Body (Str, Buff) as Body
import Isomers.Request (ServerRequest) as Request
import Isomers.Spec (Spec) as Spec
import Type.Row (type (+))

type Body = (Body.Buff + Body.Str + ())

type Spec route ireq oreq res = Spec.Spec Body route ireq oreq res

type Root ireq oreq res = Spec {} ireq oreq res

type ServerRequest = Request.ServerRequest Body

