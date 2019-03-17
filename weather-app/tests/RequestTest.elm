module RequestTest exposing (suite)

import Data.Location exposing (Location)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Decoder)
import Test exposing (..)


suite : Test
suite =
    describe "Check weather api"
        [ test "decoding apiResponse" <|
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
        ]
