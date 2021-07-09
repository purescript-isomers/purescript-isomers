module Isomers.Node.Response.Okayish.Duplexes where

-- | Fully polymorphic on content-type file response
-- unsafeFile ∷ ∀ mime. Duplex mime (Okayish () FilePath) Web.File.Blob
-- unsafeFile = ok $ _Newtype (Duplex Printer.stream Parser.string)
--   where
--     fh fp = Printer.header hContentDisposition $ "inline; filename=" <> fp
--     printer fp = fh <> Printer.stream
-- 
