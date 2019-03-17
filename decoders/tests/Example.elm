module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Decoder)
import Main
import Test exposing (..)


type alias JsonObj =
    { key1 : String, key2 : Int }


type alias Person =
    { id : Int
    , name : String
    , isClever : Bool
    , favoriteColors : List String
    , favoriteNums : List Int
    , birthdayTuple : ( String, Int )
    }


suite : Test
suite =
    describe "Add numbers"
        [ test "adds two numbers" <|
            \_ -> Expect.equal (Main.add 2 5) 7
        , test "decoding a string" <|
            let
                json =
                    "\"hello\""

                expected =
                    Ok "hello"
            in
            \_ -> Expect.equal (Decode.decodeString Decode.string json) expected
        , test "decoding a bool" <|
            let
                json =
                    "true"

                expected =
                    Ok True
            in
            \_ -> Expect.equal (Decode.decodeString Decode.bool json) expected
        , test "decoding an object" <|
            let
                json =
                    """
                    {
                        "key1": "string",
                        "key2": 11
                    }
                    """

                expected =
                    Ok { key1 = "string", key2 = 11 }

                objectDecoder =
                    Decode.map2 JsonObj
                        (Decode.field "key1" Decode.string)
                        (Decode.field "key2" Decode.int)

                fieldDecoder =
                    Decode.field "key2" Decode.int
            in
            \_ -> Expect.equal (Decode.decodeString objectDecoder json) expected
        , test "decoding a list" <|
            let
                json =
                    """
                    [[1],[2],[3, 3],[4]]
                    """

                expected =
                    Ok [ [ 1 ], [ 2 ], [ 3, 3 ], [ 4 ] ]

                decoder =
                    Decode.list (Decode.list Decode.int)
            in
            \_ -> Expect.equal (Decode.decodeString decoder json) expected
        , test "decoding a list of objects" <|
            let
                json =
                    """
                    [{
                        "key1": "string",
                        "key2": 11
                    },
                    {
                        "key1": "string2",
                        "key2": 22
                    }]
                    """

                expected =
                    Ok [ { key1 = "string", key2 = 11 }, { key1 = "string2", key2 = 22 } ]

                objectDecoder : Decoder JsonObj
                objectDecoder =
                    Decode.map2 JsonObj
                        (Decode.field "key1" Decode.string)
                        (Decode.field "key2" Decode.int)

                listDecoder : Decoder (List JsonObj)
                listDecoder =
                    Decode.list objectDecoder
            in
            \_ -> Expect.equal (Decode.decodeString listDecoder json) expected
        , test "decoding a tuple" <|
            let
                json =
                    """
                    [4, 5, "string"]
                    """

                expected =
                    Ok ( 4, 5, "string" )

                decoder =
                    Decode.map3 (\a b c -> ( a, b, c ))
                        (Decode.index 0 Decode.int)
                        (Decode.index 1 Decode.int)
                        (Decode.index 2 Decode.string)
            in
            \_ -> Expect.equal (Decode.decodeString decoder json) expected
        , test "decoding person" <|
            let
                json =
                    """
                    {
                    "id": 2,
                    "name": "Gergo",
                    "isClever": false, 
                    "favoriteColors": ["red", "green", "blue"],
                    "favoriteNums": [1, 2 ,3 ,4],
                    "birthdayTuple": ["October", 26]
                    }
                    """

                expected =
                    Ok
                        { id = 2
                        , name = "Gergo"
                        , isClever = False
                        , favoriteColors = [ "red", "green", "blue" ]
                        , favoriteNums = [ 1, 2, 3, 4 ]
                        , birthdayTuple = ( "October", 26 )
                        }

                birthdayDecoder =
                    Decode.map2 (\month day -> ( month, day ))
                        (Decode.index 0 Decode.string)
                        (Decode.index 1 Decode.int)

                decoder =
                    Decode.map6 Person
                        (Decode.field "id" Decode.int)
                        (Decode.field "name" Decode.string)
                        (Decode.field "isClever" Decode.bool)
                        (Decode.field "favoriteColors" (Decode.list Decode.string))
                        (Decode.field "favoriteNums" (Decode.list Decode.int))
                        (Decode.field "birthdayTuple" birthdayDecoder)
            in
            \_ -> Expect.equal (Decode.decodeString decoder json) expected
        , test "decoding people" <|
            let
                json =
                    """
                    [
                        {
                            "id": 2,
                            "name": "Gergo",
                            "isClever": false, 
                            "favoriteColors": ["red", "green", "blue"],
                            "favoriteNums": [1, 2 ,3 ,4],
                            "birthdayTuple": ["October", 26]
                        },
                        {
                            "id": 4,
                            "name": "Anyone",
                            "isClever": true, 
                            "favoriteColors": ["black"],
                            "favoriteNums": [4, 3, 2, 1],
                            "birthdayTuple": ["January", 1]
                        }
                    ]
                    """

                expected =
                    Ok
                        [ { id = 2
                          , name = "Gergo"
                          , isClever = False
                          , favoriteColors = [ "red", "green", "blue" ]
                          , favoriteNums = [ 1, 2, 3, 4 ]
                          , birthdayTuple = ( "October", 26 )
                          }
                        , { id = 4
                          , name = "Anyone"
                          , isClever = True
                          , favoriteColors = [ "black" ]
                          , favoriteNums = [ 4, 3, 2, 1 ]
                          , birthdayTuple = ( "January", 1 )
                          }
                        ]

                birthdayDecoder =
                    Decode.map2 (\month day -> ( month, day ))
                        (Decode.index 0 Decode.string)
                        (Decode.index 1 Decode.int)

                personDecoder =
                    Decode.map6 Person
                        (Decode.field "id" Decode.int)
                        (Decode.field "name" Decode.string)
                        (Decode.field "isClever" Decode.bool)
                        (Decode.field "favoriteColors" (Decode.list Decode.string))
                        (Decode.field "favoriteNums" (Decode.list Decode.int))
                        (Decode.field "birthdayTuple" birthdayDecoder)
            in
            \_ -> Expect.equal (Decode.decodeString (Decode.list personDecoder) json) expected
        , test "decoding numbers" <|
            let
                json =
                    """
                    [[1, "egy"], [2, "kettő"], [3, "három"], [4, "négy"]]
                    """

                expected =
                    Ok [ ( 1, "egy" ), ( 2, "kettő" ), ( 3, "három" ), ( 4, "négy" ) ]

                numberDecoder =
                    Decode.map2 (\num str -> ( num, str ))
                        (Decode.index 0 Decode.int)
                        (Decode.index 1 Decode.string)
            in
            \_ -> Expect.equal (Decode.decodeString (Decode.list numberDecoder) json) expected
        ]
