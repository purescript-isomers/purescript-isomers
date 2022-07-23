module Isomers.Response.Duplex.Variant where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer) as Control.Lazy
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Data.Variant (Variant)
import Data.Variant (case_, expand, inj, on) as Variant
import Isomers.Response.Duplex.Parser (Parser(..))
import Isomers.Response.Duplex.Parser (ParsingError(..)) as Parser
import Isomers.Response.Duplex.Type (Duplex(..))
import JS.Unsafe.Stringify (unsafeStringify)
import Prim.Row (class Cons, class Union) as Row
import Type.Prelude (class IsSymbol, Proxy)

empty :: forall ct. Duplex ct (Variant ()) (Variant ())
empty = Duplex Variant.case_ $ Control.Lazy.defer \_ -> Parser $ do
  state <- ask
  throwError (Parser.Expected "Isomers.Response.Duplex.Variant.empty" (unsafeStringify state))

-- | You can use this `inj` in a similar "style" as `Variant.on` can be used.
-- |
-- | variantDuplex
-- | # injInto (Proxy ∷ Proxy "Ok") (withStatus ok200 $ json)
-- | # injInto (Proxy ∷ Proxy "BadRequest") (withStatus badRequest400 $ json)
-- | # injInto (Proxy ∷ Proxy "Redirect") (withStatus found302 $ header hLocation)
-- |
-- | I'm not able to split this into subfunctions (like `inj` and `extend`) because
-- | theses would have partial printers.
injInto
  :: forall ct l i o li lo vi vi' vo vo'
   . IsSymbol l
  => Row.Cons l o () lo
  => Row.Cons l o vo vo'
  => Row.Union vo lo vo'
  => Row.Cons l i () li
  => Row.Cons l i vi vi'
  => Row.Union vi li vi'
  => Proxy l
  -> Duplex ct i o
  -> Duplex ct (Variant vi) (Variant vo)
  -> Duplex ct (Variant vi') (Variant vo')
injInto l (Duplex prt prs) (Duplex vPrt vPrs) = Duplex vPrt' vPrs'
  where
  vPrt' = vPrt # Variant.on l prt
  vPrs' = Variant.inj l <$> prs <|> Variant.expand <$> vPrs

