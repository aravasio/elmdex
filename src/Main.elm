module Main exposing (..)

import Basics
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string)
import Task exposing (Task)



-- MODEL


type alias Model =
    { pokemon : List Pokemon
    , error : Maybe String
    }


type alias Pokemon =
    { name : String
    , id : Int
    }


init : Model
init =
    { pokemon = []
    , error = Nothing
    }



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
        { url = url, expect = Http.expectJson PokemonFetched pokemonListDecoder }


pokemonListDecoder : Decoder (List Pokemon)
pokemonListDecoder =
    list
        (Json.Decode.map2 Pokemon
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "id" Json.Decode.int)
        )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ class "fetch-button", onClick FetchPokemon ] [ text "Fetch Pokemon" ]
        , case model.error of
            Just errorMsg ->
                div [ class "error-message" ] [ text errorMsg ]

            Nothing ->
                div [ class "pokemon-list" ] (List.map viewPokemon model.pokemon)
        ]


viewPokemon : Pokemon -> Html Msg
viewPokemon pokemon =
    div [] [ text ("Name: " ++ pokemon.name), text ("ID: " ++ String.fromInt pokemon.id) ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> update msg model
        , view = view
        , subscriptions = \_ -> Sub.none
        }
