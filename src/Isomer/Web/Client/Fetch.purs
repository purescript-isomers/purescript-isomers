module Isomer.Web.Client.Fetch where

-- import Request.Duplex (RequestDuplex')
-- 
-- fetch :: forall content route. RequestDuplex' route -> Request.Duplex.Request route -> Aff (FetchError \/ HTTP.Response String)
-- fetch duplex request = do
--   let
--     encoded = Routing.Duplex.print duplex request.route
--   raw ← case encoded.method of
--     Get →
--       Milkis.fetch
--         Milkis.fetchImpl
--         (Milkis.URL url)
--         { method: Milkis.getMethod
--         , redirect: Milkis.redirectManual
--         }
--     Post payload → do
--       Milkis.fetch
--         Milkis.fetchImpl
--         (Milkis.URL url)
--         { body: encoded.content -- fromMaybe "" (FormUrlEncoded.encode <<< Query.toFormURLEncoded =<< payload)
--         , method: Milkis.postMethod
--         , redirect: Milkis.redirectManual
--         }
--   Milkis.attachment raw <|> Milkis.json raw <#> case _ of
--     Just content → do
--       let
--         statusCode = Milkis.statusCode raw
--       Right { content, statusCode }
--     Nothing → Left ("Unkown response type: " <> unsafeStringify raw)
-- 
-- -- exchange :: forall content route.  RouteDuplex' route -> Request route -> Aff (Exchange' route (Milkis.Json + Milkis.Attachment + content))
-- -- exchange duplex request = do
-- --   response ← fetch duplex request
-- --   pure $ Exchange { request, response: Just response }