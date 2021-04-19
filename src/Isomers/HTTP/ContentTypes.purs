module Isomers.HTTP.ContentTypes where

import Data.Functor.Variant (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type JavascriptMime = "application/javascript"

type JavaScript a cts = ("application/javascript" ∷ a | cts)

_javascript = SProxy ∷ SProxy JavascriptMime

type HtmlMime = "text/html"

type Html a cts = ("text/html" ∷ a | cts)

_html = SProxy ∷ SProxy HtmlMime

type Jpeg a cts = ("image/jpeg" ∷ a | cts)

_jpeg = SProxy ∷ SProxy "image/jpeg"

jpeg ∷ ∀ a cts. a → Variant (Jpeg a + cts)
jpeg =inj _jpeg

type JsonMime = "application/json"

type Json a cts = ("application/json" ∷ a | cts)

_json = SProxy ∷ SProxy JsonMime

json ∷ ∀ a cts. a → Variant (Json a + cts)
json =inj _json

type Png a cts = ("image/png" ∷ a | cts)

_png = SProxy ∷ SProxy "image/png"

png ∷ ∀ a cts. a → Variant (Png a + cts)
png = inj _png

type Svg a cts = ("image/svg+xml" ∷ a | cts)

_svg = SProxy ∷ SProxy "image/svg+xml"

svg ∷ ∀ a cts. a → Variant (Svg a + cts)
svg = inj _svg

type Text a cts = ("text/plain" ∷ a | cts)

type TextMime = "text/plain"

_text = SProxy ∷ SProxy TextMime

text ∷ ∀ a cts. a → Variant (Text a + cts)
text = inj _text

