module Main exposing (Model, Msg(..), SearchResult, init, listSearchResults, main, onKeyUp, searchBooks, searchResultDecoder, searchResultRow, searchResultsDecoder, subscriptions, update, view)

import Browser
import Debug exposing (toString)
import Html exposing (Attribute, Html, br, button, div, h2, h3, input, li, text, ul)
import Html.Attributes exposing (placeholder, size, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map, map4, maybe, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { query : String
    , results : List SearchResult
    , error : String
    }


type alias SearchResult =
    { title : String
    , author_name : Maybe (List String)
    , isbn : Maybe (List String)
    , seed : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = Reset
    | SetQuery String
    | Search
    | GotSearchResult (Result Http.Error (List SearchResult))
    | KeyUp Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | query = "" }
            , Cmd.none
            )

        SetQuery q ->
            ( { model | query = q }
                |> Debug.log "SetQuery"
            , Cmd.none
            )

        Search ->
            ( model
                |> Debug.log "Search"
            , searchBooks model.query
            )

        GotSearchResult result ->
            case result of
                Ok results ->
                    ( { model | results = results }
                        |> Debug.log "Results"
                    , Cmd.none
                    )

                Err err ->
                    ( { model | results = [], error = toString err }
                    , Cmd.none
                    )

        KeyUp key ->
            if key == 13 then
                ( model
                , searchBooks model.query
                )

            else
                ( model
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Book Search" ]
        , div []
            [ input [ placeholder "search", size 50, value model.query, onInput SetQuery, onKeyUp KeyUp ] []
            , button [ onClick Search ] [ text "Search" ]
            ]
        , button [ onClick Reset ] [ text "Reset" ]
        , listSearchResults model.results
        ]


listSearchResults : List SearchResult -> Html Msg
listSearchResults results =
    if List.length results > 0 then
        div []
            [ h2 [] [ text "Search Results" ]
            , ul [] (List.map searchResultRow results)
            ]

    else
        div [] []


searchResultRow : SearchResult -> Html Msg
searchResultRow result =
    li []
        [ h3 [] [ text result.title ]
        , br [] []
        , text (String.join ", " result.seed)
        , br [] []
        , case result.author_name of
            Nothing ->
                text "No authors found"

            Just authors ->
                authors
                    |> String.join ","
                    |> text
        , br [] []
        , case result.isbn of
            Nothing ->
                text "No ISBNs found"

            Just isbns ->
                isbns
                    |> String.join ", "
                    |> text
        ]


searchBooks : String -> Cmd Msg
searchBooks query =
    -- https://openlibrary.org/dev/docs/api/search
    -- this endpoint can be paginated by appending page (ie. ?q=hello&page=2)
    -- returns up to 100 results per query
    Http.get
        { url = "https://openlibrary.org/search.json?q=" ++ query
        , expect = Http.expectJson GotSearchResult searchResultsDecoder
        }


searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    map4 SearchResult
        (field "title" string)
        (maybe (field "author_name" (list string)))
        (maybe (field "isbn" (list string)))
        (field "seed" (list string))


searchResultsDecoder : Decoder (List SearchResult)
searchResultsDecoder =
    field "docs" (list searchResultDecoder)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (map tagger keyCode)
