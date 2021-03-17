module Test.Request where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (SProxy(..), Variant, inj)
import Data.Variant (inj, match) as Variant
import Debug.Trace (traceM)
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (hfoldl)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Isomer.Api.Spec (Spec(..), endpoint, method, prefix) as Spec
import Isomer.Contrib.Heterogeneous (hmap', hmapWithIndex')
import Isomer.HTTP.Request.Accept (Accept(..))
import Isomer.HTTP.Request.Accept (json) as Accept
import Isomer.HTTP.Request.Data (Data(..))
import Isomer.HTTP.Request.Duplex (dataRecordInsert, dataTupleCons)
import Isomer.HTTP.Method (Method(..))
import Isomer.HTTP.Request.Method (get) as Method
import Prim.Row (class Cons) as Row
import Request.Duplex (RequestDuplex(..), RequestDuplex', int, segment)
import Request.Duplex (int, parse, segment, string) as Request.Duplex
import Request.Duplex.Printer (RequestPrinter(..))
import Type.Prelude (class IsSymbol, reflectSymbol)

-- addPrefix x = 8 /\ x
-- 
-- -- printMethod =
-- --   Variant.match
-- --     { "GET": const $ inj _get "GET"
-- --     , "POST": const $ inj _post "POST"
-- --     }
-- req ∷ Method (Variant ( "GET" ∷ Accept ( "text/json" ∷ Data Unit ), "POST" ∷ Accept ( "*/*" ∷ Data String ) ))
-- req = Method.get (Accept.json (Data unit))
-- 
-- -- z ::
-- --   Method
-- --     ( "GET" ::
-- --         Accept
-- --           ( "text/json" :: Data (Tuple Int Unit)
-- --           )
-- --     , "POST" ::
-- --         Accept
-- --           ( "*/*" :: Data (Tuple Int String)
-- --           )
-- --     )
-- -- z = hmap' (DataMapping addPrefix) req
-- req' ∷ Variant ( "admin" ∷ Method (Variant ( "GET" ∷ Accept ( "text/json" ∷ Data Unit ), "POST" ∷ Accept ( "*/*" ∷ Data String ) ) ))
-- req' = inj (SProxy ∷ SProxy "admin") req
-- 
-- -- y = hmap' (MethodApply printMethod) req'
-- -- 
-- -- k = hmap' (MethodApply printMethod) req
-- -- 
-- -- data PrintMapContent
-- --   = PrintMapContent
-- -- 
-- -- instance mappingPrintData ∷ Mapping PrintMapContent a String where
-- --   mapping _ v = unsafeStringify v
-- -- 
-- -- y' ::
-- --   forall t23.
-- --   Variant
-- --     ( admin ::
-- --         Method
-- --           ( "GET" :: String
-- --           , "POST" :: String
-- --           )
-- --     | t23
-- --     )
-- -- y' = hmap (MethodMapping PrintMapContent) req'
-- -- 
-- -- -- y'' = hmap (DataMapping unsafeStringify) req'
-- -- 
-- -- k' = hmap' (MethodMapping PrintMapContent) req
-- -- 
-- -- data PrintLabel
-- --   = PrintLabel
-- -- 
-- -- instance mappingPrintLabel ∷ (IsSymbol l) ⇒ MappingWithIndex PrintLabel (SProxy l) a String where
-- --   mappingWithIndex _ l v = reflectSymbol l
-- -- 
-- -- k'' = hmapWithIndex' (MethodMappingWithIndex PrintLabel) req'
-- -- 
-- -- k''' = hmapWithIndex' (AcceptMappingWithIndex PrintLabel) req'
-- m ∷ Method (Variant ( "GET" ∷ Data Unit, "POST" ∷ Data String ))
-- m = Method.get (Data unit)
-- 
-- vm ∷ Variant ( admin ∷ Method (Variant ( "GET" ∷ Data Unit, "POST" ∷ Data String )), home ∷ Method (Variant ( "GET" ∷ Data Unit ) ))
-- vm = inj (SProxy ∷ SProxy "admin") m
-- 
-- rdvm ::
--   RequestDuplex'
--     ( Variant
--         ( admin ::
--             Method
--               (Variant
--                 ( "GET" :: Data Unit
--                 , "POST" :: Data String
--                 )
--               )
--         , home :: Method (Variant ( "GET" :: Data Unit ))
--         )
--     )
-- rdvm = (RequestDuplex (const $ mempty) (pure vm))
-- 
-- rdm = (RequestDuplex (const $ mempty) (pure m))
-- 
-- x = dataTupleCons (int segment) rdm
-- 
-- mr ∷ Method (Variant ( "GET" ∷ Data {}, "POST" ∷ Data { base ∷ String } ))
-- mr = Method.get (Data {})
-- 
-- rdm' = (RequestDuplex (const $ mempty) (pure { mr }))
-- 
-- y ::
--   RequestDuplex
--     { mr ::
--         Method
--           (Variant
--             ( "GET" ::
--                 Data
--                   { myInt :: Int
--                   }
--             , "POST" ::
--                 Data
--                   { base :: String
--                   , myInt :: Int
--                   }
--             )
--           )
--     }
--     { mr ::
--         Method
--           (Variant
--             ( "GET" ::
--                 Data
--                   { myInt :: Int
--                   }
--             , "POST" ::
--                 Data
--                   { base :: String
--                   , myInt :: Int
--                   }
--             )
--           )
--     }
-- y = dataRecordInsert (SProxy ∷ SProxy "myInt") (int segment) rdm'
-- 
-- number ∷ Spec.ResponseCodec' Number
-- number = Spec.ResponseCodec show Number.fromString
-- 
-- string ∷ Spec.ResponseCodec' String
-- string = Spec.ResponseCodec identity Just
-- 
-- -- spec :: Spec
-- --    (Variant
-- --       ( "admin.test" :: Method
-- --                           (Variant
-- --                              ( "GET" :: Data Int
-- --                              , "POST" :: Data String
-- --                              )
-- --                           )
-- --       , "public.fest" :: Method
-- --                            (Variant
-- --                               ( "GET" :: Data Int
-- --                               , "POST" :: Data String
-- --                               )
-- --                            )
-- --       )
-- --    )
-- --    { "admin.test" :: Method
-- --                        { "GET" :: ResponseCodec Number Number
-- --                        , "POST" :: ResponseCodec String String
-- --                        }
-- --    , "public.fest" :: Method
-- --                         { "GET" :: ResponseCodec Number Number
-- --                         , "POST" :: ResponseCodec String String
-- --                         }
-- --    }
-- spec =
--   Spec.prefix
--     { "admin":
--         { test:
--             Spec.method
--               { "GET": Spec.endpoint (Request.Duplex.int Request.Duplex.segment) number
--               , "POST": Spec.endpoint (Request.Duplex.string Request.Duplex.segment) string
--               }
--         }
--     , "public":
--         { fest:
--             Spec.method
--               { "GET": Spec.endpoint (Request.Duplex.int Request.Duplex.segment) number
--               , "POST": Spec.endpoint (Request.Duplex.string Request.Duplex.segment) string
--               }
--         }
--     }
-- 
-- xxx =
--   let
--     Spec.Spec { request } = spec
-- 
--     z = Request.Duplex.parse request { method: "GET", path: "admin/public/8", headers: mempty, body: "" }
-- 
--     x = do
--       traceM z
--       Nothing
--   in
--     z
-- 
-- -- type Request
-- --   = { body :: String
-- --     , headers :: RequestHeaders
-- --     , method :: String
-- --     , path :: String
-- --     }
