module Isomers.Response.Types where

import Data.Newtype (class Newtype)
import Node.Buffer.Immutable (ImmutableBuffer) as Node.Buffer
import Web.File.Blob (Blob) as Web.File.Blob

newtype HtmlString = HtmlString String
derive instance newtypeHtml ∷ Newtype HtmlString _

newtype JavascriptString = JavascriptString String
derive instance newtypeJavascriptString ∷ Newtype JavascriptString _

data AnyBlob
  = NodeImmutableBuffer Node.Buffer.ImmutableBuffer
  | WebBlob Web.File.Blob.Blob
