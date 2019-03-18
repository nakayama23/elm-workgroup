module Data.Weather exposing (Weather, consolidatedWeatherDecoder, requestWeather, weatherDecoder)

import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder exposing (crossOrigin)


type alias Weather =
    { applicableDate : String
    , weatherStateName : String
    , weatherStateAbbr : String
    , theTemp : Float
    , minTemp : Float
    , maxTemp : Float
    , humidity : Int
    }


weatherDecoder : Decoder Weather
weatherDecoder =
    Decode.map7 Weather
        (Decode.field "applicable_date" Decode.string)
        (Decode.field "weather_state_name" Decode.string)
        (Decode.field "weather_state_abbr" Decode.string)
        (Decode.field "the_temp" Decode.float)
        (Decode.field "min_temp" Decode.float)
        (Decode.field "max_temp" Decode.float)
        (Decode.field "humidity" Decode.int)


consolidatedWeatherDecoder : Decoder (List Weather)
consolidatedWeatherDecoder =
    Decode.field "consolidated_weather" (Decode.list weatherDecoder)


requestWeather : (WebData (List Weather) -> msg) -> String -> Cmd msg
requestWeather toMsg woeid =
    Http.get
        { url =
            crossOrigin
                "https://bypasscors.herokuapp.com/api/?url=https://www.metaweather.com/api/location"
                [ "location", woeid ]
                []
        , expect = Http.expectJson (RemoteData.fromResult >> toMsg) consolidatedWeatherDecoder
        }
