module Isomers.Response.Types where

import Data.Newtype (class Newtype)

newtype HtmlString = HtmlString String
derive instance newtypeHtml ∷ Newtype HtmlString _

