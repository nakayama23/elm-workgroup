module RequestTest exposing (suite)

import Data.Location exposing (Location)
import Data.Weather exposing (Weather)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Decoder)
import Test exposing (..)


suite : Test
suite =
    describe "Check weather api"
        [ test "decoding Location Search apiResponse" <|
            let
                json =
                    """
                    [{
                        "title": "San Francisco"
                        ,"woeid": 2487956
                    },
                    {
                        "title": "San Diego"
                        ,"woeid": 2487889
                    }]
                    """

                expected =
                    Ok
                        [ { title = "San Francisco"
                          , woeid = 2487956
                          }
                        , { title = "San Diego"
                          , woeid = 2487889
                          }
                        ]
            in
            \_ -> Expect.equal (Decode.decodeString Data.Location.apiDecoder json) expected
        , test "decoding Weather apiResponse" <|
            let
                json =
                    """
                    {
                        "id": 6556085890580480,
                        "weather_state_name": "Showers",
                        "weather_state_abbr": "s",
                        "wind_direction_compass": "SSW",
                        "created": "2019-03-06T02:05:47.549973Z",
                        "applicable_date": "2019-03-06",
                        "min_temp": 8.086666666666668,
                        "max_temp": 15.51,
                        "the_temp": 12.89,
                        "wind_speed": 6.036510790457727,
                        "wind_direction": 209.74529910188946,
                        "air_pressure": 1018.87,
                        "humidity": 65,
                        "visibility": 13.315363278453828,
                        "predictability": 73
                    }
                    """

                expected =
                    Ok
                        { applicableDate = "2019-03-06"
                        , weatherStateName = "Showers"
                        , weatherStateAbbr = "s"
                        , theTemp = 12.89
                        , minTemp = 8.086666666666668
                        , maxTemp = 15.51
                        , humidity = 65
                        }
            in
            \_ -> Expect.equal (Decode.decodeString Data.Weather.weatherDecoder json) expected
        , test "decoding Weathers" <|
            let
                json =
                    """
                    {
                        "consolidated_weather": [
                            {
                            "weather_state_name": "Showers",
                            "weather_state_abbr": "s",
                            "applicable_date": "2019-03-06",
                            "min_temp": 8.086666666666668,
                            "max_temp": 15.51,
                            "the_temp": 12.89,
                            "humidity": 65
                            },
                            {
                            "weather_state_name": "Light Rain",
                            "weather_state_abbr": "lr",
                            "applicable_date": "2019-03-07",
                            "min_temp": 5.873333333333334,
                            "max_temp": 11.13,
                            "the_temp": 10.935,
                            "humidity": 80
                            }
                        ]
                    }
                    """

                expected : Result error (List Weather)
                expected =
                    Ok
                        [ { applicableDate = "2019-03-06"
                          , weatherStateName = "Showers"
                          , weatherStateAbbr = "s"
                          , theTemp = 12.89
                          , minTemp = 8.086666666666668
                          , maxTemp = 15.51
                          , humidity = 65
                          }
                        , { applicableDate = "2019-03-07"
                          , weatherStateName = "Light Rain"
                          , weatherStateAbbr = "lr"
                          , theTemp = 10.935
                          , minTemp = 5.873333333333334
                          , maxTemp = 11.13
                          , humidity = 80
                          }
                        ]
            in
            \_ -> Expect.equal (Decode.decodeString Data.Weather.consolidatedWeatherDecoder json) expected
        ]
