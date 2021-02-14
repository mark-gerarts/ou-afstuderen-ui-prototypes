module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, label, option, select, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = NoOp


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm" ]
        , h2 [] [ text "Album form" ]
        , viewAlbumForm
        ]


viewAlbumForm : Html Msg
viewAlbumForm =
    form []
        [ div []
            [ label [] [ text "Name" ]
            , input [] []
            ]
        , div []
            [ label [] [ text "Artist" ]
            , input [] []
            ]
        , div []
            [ label [] [ text "Source" ] ]
        , select []
            [ option [] [ text "CD" ]
            , option [] [ text "LP" ]
            , option [] [ text "Digital" ]
            ]
        , div []
            [ label [] [ text "Tags" ]
            , div []
                [ input [ style "display" "inline-block" ] []
                , button [ style "display" "inline-block" ] [ text "Remove" ]
                ]
            , button [] [ text "Add more" ]
            ]
        , button [] [ text "submit" ]
        ]
