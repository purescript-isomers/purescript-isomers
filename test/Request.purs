module Test.Request where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (SProxy(..), Variant, inj)
import Data.Variant (inj, match) as Variant
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Folding (hfoldl)
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Hybrid.Api.Spec (Raw(..), ResponseCodec(..), ResponseCodec', endpoint, method, prefix) as Spec
import Hybrid.Contrib.Heterogeneous (hmap', hmapWithIndex')
import Hybrid.HTTP.Request.Accept (Accept(..))
import Hybrid.HTTP.Request.Accept (json) as Accept
import Hybrid.HTTP.Request.Data (Data(..))
import Hybrid.HTTP.Request.Duplex (dataRecordInsert, dataTupleCons)
import Hybrid.HTTP.Request.Method (Method(..))
import Hybrid.HTTP.Request.Method (get) as Method
import Prim.Row (class Cons) as Row
import Request.Duplex (RequestDuplex(..), RequestDuplex', int, segment)
import Request.Duplex (int, segment, string) as Request.Duplex
import Request.Duplex.Printer (RequestPrinter(..))
import Type.Prelude (class IsSymbol, reflectSymbol)

addPrefix x = 8 /\ x

-- printMethod =
--   Variant.match
--     { "GET": const $ inj _get "GET"
--     , "POST": const $ inj _post "POST"
--     }
req ∷ Method ( "GET" ∷ Accept ( "text/json" ∷ Data Unit ), "POST" ∷ Accept ( "*/*" ∷ Data String ) )
req = Method.get (Accept.json (Data unit))

-- z ::
--   Method
--     ( "GET" ::
--         Accept
--           ( "text/json" :: Data (Tuple Int Unit)
--           )
--     , "POST" ::
--         Accept
--           ( "*/*" :: Data (Tuple Int String)
--           )
--     )
-- z = hmap' (DataMapping addPrefix) req
req' ∷ Variant ( "admin" ∷ Method ( "GET" ∷ Accept ( "text/json" ∷ Data Unit ), "POST" ∷ Accept ( "*/*" ∷ Data String ) ) )
req' = inj (SProxy ∷ SProxy "admin") req

-- y = hmap' (MethodApply printMethod) req'
-- 
-- k = hmap' (MethodApply printMethod) req
-- 
-- data PrintMapContent
--   = PrintMapContent
-- 
-- instance mappingPrintData ∷ Mapping PrintMapContent a String where
--   mapping _ v = unsafeStringify v
-- 
-- y' ::
--   forall t23.
--   Variant
--     ( admin ::
--         Method
--           ( "GET" :: String
--           , "POST" :: String
--           )
--     | t23
--     )
-- y' = hmap (MethodMapping PrintMapContent) req'
-- 
-- -- y'' = hmap (DataMapping unsafeStringify) req'
-- 
-- k' = hmap' (MethodMapping PrintMapContent) req
-- 
-- data PrintLabel
--   = PrintLabel
-- 
-- instance mappingPrintLabel ∷ (IsSymbol l) ⇒ MappingWithIndex PrintLabel (SProxy l) a String where
--   mappingWithIndex _ l v = reflectSymbol l
-- 
-- k'' = hmapWithIndex' (MethodMappingWithIndex PrintLabel) req'
-- 
-- k''' = hmapWithIndex' (AcceptMappingWithIndex PrintLabel) req'
m ∷ Method ( "GET" ∷ Data Unit, "POST" ∷ Data String )
m = Method.get (Data unit)

vm ∷ Variant ( admin ∷ Method ( "GET" ∷ Data Unit, "POST" ∷ Data String ), home ∷ Method ( "GET" ∷ Data Unit ) )
vm = inj (SProxy ∷ SProxy "admin") m

rdvm ::
  RequestDuplex'
    ( Variant
        ( admin ::
            Method
              ( "GET" :: Data Unit
              , "POST" :: Data String
              )
        , home :: Method ( "GET" :: Data Unit )
        )
    )
rdvm = (RequestDuplex (const $ mempty) (pure vm))

rdm = (RequestDuplex (const $ mempty) (pure m))

x = dataTupleCons (int segment) rdm

mr ∷ Method ( "GET" ∷ Data {}, "POST" ∷ Data { base ∷ String } )
mr = Method.get (Data {})

rdm' = (RequestDuplex (const $ mempty) (pure { mr }))

y ::
  RequestDuplex
    { mr ::
        Method
          ( "GET" ::
              Data
                { myInt :: Int
                }
          , "POST" ::
              Data
                { base :: String
                , myInt :: Int
                }
          )
    }
    { mr ::
        Method
          ( "GET" ::
              Data
                { myInt :: Int
                }
          , "POST" ::
              Data
                { base :: String
                , myInt :: Int
                }
          )
    }
y = dataRecordInsert (SProxy ∷ SProxy "myInt") (int segment) rdm'

number ∷ Spec.ResponseCodec' Number
number = Spec.ResponseCodec show Number.fromString

string ∷ Spec.ResponseCodec' String
string = Spec.ResponseCodec identity Just

spec ::
  Raw
    ( Variant
        ( "admin.test" ::
            Method
              ( "GET" :: Data Int
              , "POST" :: Data String
              )
        , "public.fest" ::
            Method
              ( "GET" :: Data Int
              , "POST" :: Data String
              )
        )
    )
    { "admin.test" ::
        MethodsResponses
          ( "GET" :: ResponseCodec Number Number
          , "POST" :: ResponseCodec String String
          )
    , "public.fest" ::
        MethodsResponses
          ( "GET" :: ResponseCodec Number Number
          , "POST" :: ResponseCodec String String
          )
    }
spec =
  Spec.prefix
    { "admin":
        { test:
            Spec.method
              { "GET": Spec.endpoint (Request.Duplex.int Request.Duplex.segment) number
              , "POST": Spec.endpoint (Request.Duplex.string Request.Duplex.segment) string
              }
        }
    , "public":
        { fest:
            Spec.method
              { "GET": Spec.endpoint (Request.Duplex.int Request.Duplex.segment) number
              , "POST": Spec.endpoint (Request.Duplex.string Request.Duplex.segment) string
              }
        }
    }
