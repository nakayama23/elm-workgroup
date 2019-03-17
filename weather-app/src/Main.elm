module Main exposing (Model, Msg(..))

import Browser
import Data.Location as Location exposing (Location)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { locations : WebData (List Location)
    , locationName : String
    }


type Msg
    = GotLocation (WebData (List Location))
    | InputLocation String
    | Search


init : () -> ( Model, Cmd Msg )
init _ =
    ( { locations = NotAsked, locationName = "tokyo" }, Location.requestLocation GotLocation "tokyo" )


view : Model -> Html Msg
view model =
    case model.locations of
        Success locations ->
            div [] (List.map viewLocation locations ++ [ textField "" ])

        _ ->
            div [] [ text "No data", textField "" ]


viewLocation : Location -> Html Msg
viewLocation location =
    div [] [ text (location.title ++ " " ++ String.fromInt location.woeid) ]


textField : String -> Html Msg
textField locationName =
    div [] [ input [ value locationName, onInput InputLocation ] [], button [] [ text "送信" ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLocation response ->
            ( { model | locations = response }, Cmd.none )

        InputLocation locationName ->
            ( { model | locationName = locationName }, Cmd.none )

        Search ->
            ( model, Cmd.none )


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
