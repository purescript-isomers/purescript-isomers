module Hybrid.Contrib.Milkis where

import Prelude

import Control.Monad.Except (catchError)
import Control.Promise (Promise)
import Control.Promise (toAffE) as Promise
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix) as String
import Data.String.Utils (trimEnd, trimStart) as String
import Data.Traversable (for)
import Data.Variant (Variant, case_, inj, on)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (lookup) as Object
import Global.Unsafe (unsafeStringify)
-- import Hybrid.HTTP (Response(..))
-- import Hybrid.HTTP.Response (Response(..)) as Hybrid.HTTP
import Milkis (Response, headers, statusCode, text) as Milkis
import Milkis.Impl (FetchImpl)
import Type.Prelude (SProxy(..))
import Unsafe.Reference (unsafeRefEq)

foreign import data JSEngine ∷ Type

-- | We have two cases
foreign import node ∷ JSEngine
foreign import browser ∷ JSEngine

-- | Current engine constant
foreign import jsEngine ∷ JSEngine

instance eqEngine ∷ Eq JSEngine where
  eq = unsafeRefEq

instance showEngine ∷ Show JSEngine where
  show e = "JSEngine (" <> unsafeStringify e <> ")"


foreign import fetchImpl ∷ FetchImpl

foreign import saveAttachmentImpl ∷ Milkis.Response → Effect (Promise Boolean)

saveAttachment ∷ Milkis.Response → Aff Boolean
saveAttachment = saveAttachmentImpl >>> Promise.toAffE

type FileName = String

-- attachment ∷ ∀ a. Milkis.Response → Aff (Maybe (Hybrid.HTTP.Response a))
-- attachment res = map (map Attachment <<< join) $ for (contentDisposition res) $ case_
--   # on _attachment (const $ Just <$> saveAttachment res)
--   # on _inline (const $ pure Nothing)
--   where
--     _attachment = SProxy ∷ SProxy "attachment"
--     _inline = SProxy ∷ SProxy "inline"
-- 
--     contentDisposition ∷ Milkis.Response → Maybe (Variant (attachment ∷ Maybe FileName, inline ∷ Unit))
--     contentDisposition resp =
--       let
--         headers = Milkis.headers resp
--         trim = String.trimEnd <<< String.trimStart
--         parse v = case map trim (String.split (String.Pattern ";") v) of
--           [ "attachment" ] → pure $ inj _attachment Nothing
--           [ "attachment", fileNameDecl ] →
--             let
--               filename = String.stripPrefix (String.Pattern "filename=") fileNameDecl
--             in
--               pure $ inj _attachment filename
--           [ "inline" ] → pure $ inj _inline unit
--           otherwise → Nothing
--       in
--         Object.lookup "content-disposition" headers >>= parse
-- 
-- redirect ∷ ∀ a. Milkis.Response → Maybe (Hybrid.HTTP.Response a)
-- redirect res = do
--   location ← Object.lookup "location" $ Milkis.headers res
--   case Milkis.statusCode res of
--     301 → Just $ Hybrid.HTTP.Redirect location
--     302 → Just $ Hybrid.HTTP.Redirect location
--     otherwise → Nothing
-- 
-- response ∷ Milkis.Response → Aff (Maybe (Hybrid.HTTP.Response String))
-- response res = do
--   (Just <<< Hybrid.HTTP.Response <$> Milkis.text res) `catchError` (const $ pure Nothing)
