module Isomers.Node.Types where

import Isomers.Node.Request.Body (Buff, Str) as Body
import Type.Row (type (+))

type BodyBase body = (Body.Buff + Body.Str + body)

type SimpleBody = BodyBase ()

-- type NodeRequest body = Request.Types.ServerRequest (BodyBase body)

type NodeSimpleRequest = BodyBase ()
