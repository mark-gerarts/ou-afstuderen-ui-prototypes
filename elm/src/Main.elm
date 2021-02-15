module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, label, option, select, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias Model =
    { album : Album
    , submitted : Bool
    }


type alias Album =
    { name : String
    , artist : String
    , source : Source
    , tags : List String
    }


type Source
    = CD
    | LP
    | Digital


type Msg
    = NoOp
    | AddTag
    | RemoveTag Int
    | UpdateTag Int String
    | SubmitForm


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


initialModel : Model
initialModel =
    { album = exampleAlbum
    , submitted = False
    }


exampleAlbum : Album
exampleAlbum =
    { name = "The Dark Side of the Moon"
    , artist = "Pink Floyd"
    , source = Digital
    , tags = [ "Rock" ]
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        AddTag ->
            { model | album = addTag model.album "" }

        RemoveTag i ->
            { model | album = removeTag model.album i }

        UpdateTag i newTag ->
            { model | album = updateTag model.album i newTag }

        SubmitForm ->
            -- @todo: http request
            { model | submitted = True }


addTag : Album -> String -> Album
addTag album tag =
    { album | tags = album.tags ++ [ tag ] }


removeTag : Album -> Int -> Album
removeTag album i =
    { album | tags = List.take i album.tags ++ List.drop (i + 1) album.tags }


updateTag : Album -> Int -> String -> Album
updateTag album i newTag =
    { album | tags = List.take i album.tags ++ [ newTag ] ++ List.drop (i + 1) album.tags }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm" ]
        , h2 [] [ text "Album form" ]
        , viewAlbumForm model
        ]


viewAlbumForm : Model -> Html Msg
viewAlbumForm { album, submitted } =
    div []
        [ div []
            [ label [] [ text "Name" ]
            , input [ value album.name ] []
            ]
        , div []
            [ label [] [ text "Artist" ]
            , input [ value album.artist ] []
            ]
        , div []
            [ label [] [ text "Source" ] ]
        , select []
            (List.map (viewSourceOption album.source) [ CD, LP, Digital ])
        , div []
            (List.concat
                [ [ label [] [ text "Tags" ] ]
                , List.indexedMap (\i t -> viewTag i t) album.tags
                , [ button [ onClick AddTag ] [ text "Add more" ] ]
                ]
            )
        , button
            [ disabled submitted
            , onClick SubmitForm
            ]
            [ text
                (if submitted then
                    "Submitted!"

                 else
                    "Submit"
                )
            ]
        ]


viewSourceOption : Source -> Source -> Html Msg
viewSourceOption activeSource source =
    option
        [ value (sourceToString source)
        , selected (source == activeSource)
        ]
        [ text (sourceToString source) ]


sourceToString : Source -> String
sourceToString source =
    case source of
        CD ->
            "CD"

        LP ->
            "LP"

        Digital ->
            "Digital"


viewTag : Int -> String -> Html Msg
viewTag i tag =
    div []
        [ input
            [ style "display" "inline-block"
            , value tag
            , onInput (UpdateTag i)
            ]
            []
        , button
            [ style "display" "inline-block"
            , onClick (RemoveTag i)
            ]
            [ text "Remove" ]
        ]
