module Hybrid.HTTP.ContentTypes where

import Data.Functor.Variant (SProxy(..))
import Data.Variant (Variant, inj)
import Type.Row (type (+))

type Jpeg a cts = ("image/jpeg" ∷ a | cts)

type Json a cts = ("text/json" ∷ a | cts)

type Png a cts = ("image/png" ∷ a | cts)

type Svg a cts = ("image/svg+xml" ∷ a | cts)

_jpeg = SProxy ∷ SProxy "image/jpeg"

_json = SProxy ∷ SProxy "text/json"

_png = SProxy ∷ SProxy "image/png"

_svg = SProxy ∷ SProxy "image/svg+xml"

jpeg ∷ ∀ a cts. a → Variant (Jpeg a + cts)
jpeg =inj _jpeg

json ∷ ∀ a cts. a → Variant (Json a + cts)
json =inj _json

png ∷ ∀ a cts. a → Variant (Png a + cts)
png = inj _png

svg ∷ ∀ a cts. a → Variant (Svg a + cts)
svg = inj _svg


