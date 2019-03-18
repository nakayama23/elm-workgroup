module Main exposing (Model, Msg(..))

-- import Data.Icon as Icon exposing (Icon)

import Browser
import Data.Location as Location exposing (Location)
import Data.Weather as Weather exposing (Weather)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { locations : WebData (List Location)
    , weather : WebData (List Weather)
    , locationName : String
    }


type Msg
    = GotLocation (WebData (List Location))
    | GotWeather (WebData (List Weather))
    | InputLocation String
    | Search


init : () -> ( Model, Cmd Msg )
init _ =
    ( { locations = NotAsked, weather = NotAsked, locationName = "tokyo" }, Location.requestLocation GotLocation "tokyo" )


view : Model -> Html Msg
view model =
    case model.weather of
        Success weathers ->
            case List.head weathers of
                Just weather ->
                    div [] [ viewWeather weather, textField model.locationName ]

                _ ->
                    div [] [ text "Can't get weather data", textField model.locationName ]

        _ ->
            div [] [ text "No location data", textField model.locationName ]


viewLocation : Location -> Html Msg
viewLocation location =
    div [] [ text (location.title ++ " " ++ String.fromInt location.woeid) ]


viewWeather : Weather -> Html Msg
viewWeather weather =
    div []
        [ img [ src <| getIconUrl weather.weatherStateAbbr, width 50, height 50 ] []
        , text
            (String.join " "
                [ weather.applicableDate
                , weather.weatherStateName
                , String.fromFloat weather.theTemp
                , String.fromFloat weather.minTemp
                , String.fromFloat weather.maxTemp
                , String.fromInt weather.humidity
                ]
            )
        ]


getIconUrl : String -> String
getIconUrl abbr =
    "https://www.metaweather.com/static/img/weather/" ++ abbr ++ ".svg"


textField : String -> Html Msg
textField locationName =
    div [] [ input [ value locationName, onInput InputLocation ] [], button [ onClick Search ] [ text "送信" ] ]


requestWeather : Model -> Cmd Msg
requestWeather model =
    case model.locations of
        Success locations ->
            case List.head locations of
                Just location ->
                    Weather.requestWeather GotWeather (String.fromInt location.woeid)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none



-- requestIcon : Model -> Cmd Msg
-- requestIcon model =
--     case model.weather of
--         Success weathers ->
--             case List.head weathers of
--                 Just weather ->
--                     Icon.requestIcon GotIcon weather.weatherStateAbbr
--                 _ ->
--                     Cmd.none
--         _ ->
--             Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLocation response ->
            ( { model | locations = response }, requestWeather model )

        GotWeather response ->
            ( { model | weather = response }, Cmd.none )

        InputLocation locationName ->
            ( { model | locationName = locationName }, Cmd.none )

        Search ->
            ( model, Location.requestLocation GotLocation model.locationName )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
