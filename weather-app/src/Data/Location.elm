module Data.Location exposing (Location, apiDecoder, locationDecoder, requestLocation)

import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder exposing (crossOrigin)


type alias Location =
    { title : String
    , woeid : Int
    }


locationDecoder =
    Decode.map2 Location
        (Decode.field "title" Decode.string)
        (Decode.field "woeid" Decode.int)


requestLocation : (WebData (List Location) -> msg) -> String -> Cmd msg
requestLocation toMsg query =
    Http.get
        { url =
            crossOrigin
                "https://bypasscors.herokuapp.com/api/?url=https://www.metaweather.com/api"
                [ "location", "search" ]
                [ Url.Builder.string "query" query ]
        , expect = Http.expectJson (RemoteData.fromResult >> toMsg) apiDecoder
        }


apiDecoder =
    Decode.list locationDecoder
