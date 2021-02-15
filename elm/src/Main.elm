module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, h2, input, label, option, select, source, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode


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
    | FormSubmitted (Result Http.Error ())


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTag ->
            ( { model | album = addTag model.album "" }, Cmd.none )

        RemoveTag i ->
            ( { model | album = removeTag model.album i }, Cmd.none )

        UpdateTag i newTag ->
            ( { model | album = updateTag model.album i newTag }, Cmd.none )

        SubmitForm ->
            ( model, submitAlbum model.album )

        FormSubmitted _ ->
            ( { model | submitted = True }, Cmd.none )


addTag : Album -> String -> Album
addTag album tag =
    { album | tags = album.tags ++ [ tag ] }


removeTag : Album -> Int -> Album
removeTag album i =
    { album | tags = List.take i album.tags ++ List.drop (i + 1) album.tags }


updateTag : Album -> Int -> String -> Album
updateTag album i newTag =
    { album | tags = List.take i album.tags ++ [ newTag ] ++ List.drop (i + 1) album.tags }


submitAlbum : Album -> Cmd Msg
submitAlbum album =
    Http.post
        { url = "http://localhost:8080"
        , body = Http.jsonBody (encode album)
        , expect = Http.expectWhatever FormSubmitted
        }


encode : Album -> JsonEncode.Value
encode album =
    JsonEncode.object
        [ ( "name", JsonEncode.string album.name )
        , ( "artist", JsonEncode.string album.artist )
        , ( "source", JsonEncode.string (sourceToString album.source) )
        , ( "tags", JsonEncode.list JsonEncode.string album.tags )
        ]


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
