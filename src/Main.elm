module Main exposing (main)

import Browser
import Dict
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Platform.Cmd exposing (Cmd)
import Html exposing (h2)


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
    ( { monstersList = [], currentMonster = Nothing }
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

        GetMonster url ->
            ( model
            , Http.get
                { url = toApi url
                , expect = Http.expectJson GotMonster monsterDecoder
                }
            )

        GotMonster result ->
            case result of
                Err x ->
                    let
                        y =
                            Debug.log "Errr" x
                    in
                    ( model, Cmd.none )

                Ok monster ->
                    ( { model | currentMonster = Just monster }, Cmd.none )


type alias Model =
    { monstersList : List MonsterHeader
    , currentMonster : Maybe Monster
    }


type alias MonsterHeader =
    { index : String
    , name : String
    , url : String
    }


type alias Monster =
    { name : String
    , size : String
    }


type Msg
    = GotMonstersList (Result Http.Error (List MonsterHeader))
    | GetMonster String
    | GotMonster (Result Http.Error Monster)


monstersDecoder : Decoder (List MonsterHeader)
monstersDecoder =
    field "results"
        (Json.Decode.list monsterHeaderDecoder)


monsterHeaderDecoder : Decoder MonsterHeader
monsterHeaderDecoder =
    Json.Decode.map3 MonsterHeader
        (field "index" string)
        (field "name" string)
        (field "url" string)


monsterDecoder : Decoder Monster
monsterDecoder =
    Json.Decode.map2 Monster
        (field "name" string)
        (field "size" string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ case model.currentMonster of
                Nothing ->
                    text "No monsters loaded..."

                Just monster ->
                    viewMonster monster
            , div [] (List.map viewMonsterHeader model.monstersList)
            ]
        ]


viewMonsterHeader : MonsterHeader -> Html Msg
viewMonsterHeader { index, name, url } =
    p [ Html.Attributes.title index, onClick (GetMonster url) ] [ text name ]

viewMonster: Monster -> Html Msg
viewMonster monster=
 div [] [
     h2 [] [text monster.name]
     ,Html.h6 [] [text ("Size:"  ++ monster.size)]
 ]
