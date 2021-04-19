module Isomers.Response.Types where

import Data.Newtype (class Newtype)

newtype HtmlString = HtmlString String
derive instance newtypeHtml ∷ Newtype HtmlString _

newtype JavascriptString = JavascriptString String
derive instance newtypeJavascriptString ∷ Newtype JavascriptString _
