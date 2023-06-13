module Main exposing (..)

import Basics
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string)
import Maybe
import Task exposing (Task)
import Url exposing (..)
import Url.Parser exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init ""
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


init : String -> ( Model, Cmd Msg )
init url =
    let
        initialRoute =
            url
                |> Url.fromString
                |> Maybe.andThen (\parsedUrl -> Url.Parser.parse routeParser parsedUrl)
                |> Maybe.withDefault PokemonList
    in
    ( { pokemon = []
      , error = Nothing
      , route = initialRoute
      }
    , Cmd.none
    )



-- ROUTES


type Route
    = PokemonList
    | PokemonDetail String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map PokemonList (s "")
        , map PokemonDetail (s "pokemon" </> Url.Parser.string)
        ]



-- MODEL


type alias Model =
    { route : Route
    , pokemon : List Pokemon
    , error : Maybe String
    }


type alias Pokemon =
    { name : String
    , url : String
    }


type alias PokemonListResponse =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List Pokemon
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button
            [ class "fetch-button", onClick FetchPokemon ]
            [ text "Fetch Pokemon" ]
        , case model.error of
            Just errorMsg ->
                div [ class "error-message" ] [ text errorMsg ]

            Nothing ->
                div [ class "pokemon-list" ] (List.map viewPokemon model.pokemon)
        ]


viewPokemon : Pokemon -> Html Msg
viewPokemon pokemon =
    div [] [ text ("Name: " ++ pokemon.name), text ("ID: " ++ pokemon.url) ]



-- UPDATE


type Msg
    = FetchPokemon
    | PokemonFetched (Result Http.Error (List Pokemon))


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody message ->
            "Bad body: " ++ message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPokemon ->
            ( model, fetchPokemon )

        PokemonFetched (Ok pokemon) ->
            ( { model | pokemon = pokemon }, Cmd.none )

        PokemonFetched (Err error) ->
            ( { model | error = Just (errorToString error) }, Cmd.none )


fetchPokemon : Cmd Msg
fetchPokemon =
    let
        url =
            "https://pokeapi.co/api/v2/pokemon?limit=151"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson (PokemonFetched << Result.map .results)
                pokemonListResponseDecoder
        }


pokemonListResponseDecoder : Decoder PokemonListResponse
pokemonListResponseDecoder =
    Json.Decode.map4 PokemonListResponse
        (Json.Decode.field "count" Json.Decode.int)
        (Json.Decode.field "next" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "previous" (Json.Decode.nullable Json.Decode.string))
        (Json.Decode.field "results" (Json.Decode.list pokemonDecoder))


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Json.Decode.map2 Pokemon
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "url" Json.Decode.string)
