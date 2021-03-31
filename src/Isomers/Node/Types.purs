module Isomers.Node.Types where

import Isomers.Node.Request.Body (Str, Buff) as Body
import Isomers.Request (ServerRequest) as Request
import Isomers.Spec (Spec) as Spec
import Type.Row (type (+))

type Body = (Body.Buff + Body.Str + ())

type Spec i req res = Spec.Spec Body i req res

type Root payload res = Spec {} payload res

type ServerRequest = Request.ServerRequest Body

