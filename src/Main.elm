module Main exposing (main)

import Browser
import Dict
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Platform.Cmd exposing (Cmd)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


toApi url =
    "https://www.dnd5eapi.co" ++ url


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monstersList = [] }
    , Http.get
        { url = toApi "/api/monsters"
        , expect = Http.expectJson GotMonstersList monstersDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMonstersList result ->
            case result of
                Err x ->
                    let
                        y =
                            Debug.log "Errr" x
                    in
                    ( model, Cmd.none )

                Ok monstersList ->
                    ( { model | monstersList = monstersList }, Cmd.none )


type alias Model =
    { monstersList : List MonsterHeader }


type alias MonsterHeader =
    { index : String
    , name : String
    , url : String
    }


type Msg
    = GotMonstersList (Result Http.Error (List MonsterHeader))


monstersDecoder : Decoder (List MonsterHeader)
monstersDecoder =
    field "results"
        (Json.Decode.list monsterDecoder)


monsterDecoder : Decoder MonsterHeader
monsterDecoder =
    Json.Decode.map3 MonsterHeader
        (field "index" string)
        (field "name" string)
        (field "name" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMonsterHeader model.monstersList)
        ]


viewMonsterHeader : MonsterHeader -> Html msg
viewMonsterHeader { index, name, url } =
    p [ Html.Attributes.title index ] [ text name ]
