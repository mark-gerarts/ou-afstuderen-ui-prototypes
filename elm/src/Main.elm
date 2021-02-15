module Main exposing (..)

import Browser
import Html exposing (Html, button, dd, div, dl, dt, form, h1, h2, input, label, option, select, source, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode


type alias Model =
    { album : Album
    , submitted : Bool
    , serverResponse : Maybe Album
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
    = Update UpdateAlbum
    | SubmitForm
    | FormSubmitted (Result Http.Error ())
    | FetchAlbum
    | AlbumFetched (Result Http.Error Album)


type UpdateAlbum
    = UpdateName String
    | UpdateArtist String
    | UpdateSource Source
    | AddTag
    | UpdateTag Int String
    | RemoveTag Int


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
    , serverResponse = Nothing
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
        SubmitForm ->
            ( model, submitAlbum model.album )

        FormSubmitted _ ->
            ( { model | submitted = True }, Cmd.none )

        Update albumUpdate ->
            let
                album =
                    model.album
            in
            case albumUpdate of
                UpdateName name ->
                    ( { model | album = { album | name = name } }, Cmd.none )

                UpdateArtist artist ->
                    ( { model | album = { album | artist = artist } }, Cmd.none )

                UpdateSource source ->
                    ( { model | album = { album | source = source } }, Cmd.none )

                AddTag ->
                    ( { model | album = addTag album "" }, Cmd.none )

                RemoveTag i ->
                    ( { model | album = removeTag album i }, Cmd.none )

                UpdateTag i tag ->
                    ( { model | album = updateTag album i tag }, Cmd.none )

        FetchAlbum ->
            ( model, fetchAlbum )

        AlbumFetched (Ok album) ->
            ( { model | serverResponse = Just album }, Cmd.none )

        AlbumFetched (Err _) ->
            ( model, Cmd.none )


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
        , body = Http.jsonBody (encodeAlbum album)
        , expect = Http.expectWhatever FormSubmitted
        }


encodeAlbum : Album -> JsonEncode.Value
encodeAlbum album =
    JsonEncode.object
        [ ( "name", JsonEncode.string album.name )
        , ( "artist", JsonEncode.string album.artist )
        , ( "source", JsonEncode.string (sourceToString album.source) )
        , ( "tags", JsonEncode.list JsonEncode.string album.tags )
        ]


albumDecoder : JsonDecode.Decoder Album
albumDecoder =
    JsonDecode.map4 Album
        (JsonDecode.field "name" JsonDecode.string)
        (JsonDecode.field "artist" JsonDecode.string)
        (JsonDecode.map sourceFromString (JsonDecode.field "source" JsonDecode.string))
        (JsonDecode.field "tags" (JsonDecode.list JsonDecode.string))


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Elm" ]
        , h2 [] [ text "Album form" ]
        , viewAlbumForm model
        , h2 [] [ text "Displaying JSON data" ]
        , case model.serverResponse of
            Nothing ->
                button [ onClick FetchAlbum ] [ text "Fetch album from server" ]

            Just album ->
                viewAlbum album
        ]


viewAlbumForm : Model -> Html Msg
viewAlbumForm { album, submitted } =
    div []
        [ div []
            [ label [] [ text "Name" ]
            , input
                [ value album.name
                , onInput (\v -> Update (UpdateName v))
                ]
                []
            ]
        , div []
            [ label [] [ text "Artist" ]
            , input
                [ value album.artist
                , onInput (\v -> Update (UpdateArtist v))
                ]
                []
            ]
        , div []
            [ label [] [ text "Source" ] ]
        , select [ onInput (\v -> Update (UpdateSource (sourceFromString v))) ]
            (List.map (viewSourceOption album.source) [ CD, LP, Digital ])
        , div []
            (List.concat
                [ [ label [] [ text "Tags" ] ]
                , List.indexedMap (\i t -> viewTag i t) album.tags
                , [ button [ onClick (Update AddTag) ] [ text "Add more" ] ]
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


sourceFromString : String -> Source
sourceFromString source =
    case source of
        "CD" ->
            CD

        "LP" ->
            LP

        _ ->
            Digital


viewTag : Int -> String -> Html Msg
viewTag i tag =
    div []
        [ input
            [ style "display" "inline-block"
            , value tag
            , onInput (\v -> Update (UpdateTag i v))
            ]
            []
        , button
            [ style "display" "inline-block"
            , onClick (Update (RemoveTag i))
            ]
            [ text "Remove" ]
        ]


viewAlbum : Album -> Html Msg
viewAlbum album =
    dl []
        [ dt [] [ text "Title:" ]
        , dd [] [ text album.name ]
        , dt [] [ text "Artist" ]
        , dd [] [ text album.artist ]
        , dt [] [ text "Source:" ]
        , dd [] [ text (sourceToString album.source) ]
        , dt [] [ text "Tags:" ]
        , dd [] [ text (String.join ", " album.tags) ]
        ]


fetchAlbum : Cmd Msg
fetchAlbum =
    Http.get
        { url = "http://localhost:8080/album/1"
        , expect = Http.expectJson AlbumFetched albumDecoder
        }
