module Isomers.HTTP.ContentTypes where

import Data.Variant (Variant, inj)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type JavascriptMime = "application/javascript"

type JavaScript a cts = ("application/javascript" :: a | cts)

_javascript = Proxy :: Proxy JavascriptMime

type HtmlMime = "text/html"

type Html a cts = ("text/html" :: a | cts)

_html = Proxy :: Proxy HtmlMime

type JpegMime = "image/jpeg"

type Jpeg a cts = ("image/jpeg" :: a | cts)

_jpeg = Proxy :: Proxy JpegMime

jpeg :: forall a cts. a -> Variant (Jpeg a + cts)
jpeg = inj _jpeg

type JsonMime = "application/json"

type Json a cts = ("application/json" :: a | cts)

_json = Proxy :: Proxy JsonMime

json :: forall a cts. a -> Variant (Json a + cts)
json = inj _json

type Png a cts = ("image/png" :: a | cts)

_png = Proxy :: Proxy "image/png"

png :: forall a cts. a -> Variant (Png a + cts)
png = inj _png

type Svg a cts = ("image/svg+xml" :: a | cts)

_svg = Proxy :: Proxy "image/svg+xml"

svg :: forall a cts. a -> Variant (Svg a + cts)
svg = inj _svg

type Text a cts = ("text/plain" :: a | cts)

type TextMime = "text/plain"

_text = Proxy :: Proxy TextMime

text :: forall a cts. a -> Variant (Text a + cts)
text = inj _text

