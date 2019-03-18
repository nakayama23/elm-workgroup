module Data.Icon exposing (requestIcon)

import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder exposing (crossOrigin)



-- type alias Icon =
-- { icon : String }
-- requestIcon : (WebData String -> msg) -> String -> Cmd msg
-- requestIcon toMsg abbr =
--     Http.get
--         { url =
--             crossOrigin
--                 "https://bypasscors.herokuapp.com/api/?url=https://www.metaweather.com/static/img/weather"
--                 [ abbr ++ ".svg" ]
--                 []
--         , expect = Http.expectString
--         }
