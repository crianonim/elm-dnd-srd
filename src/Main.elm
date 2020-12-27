module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, h2, p, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import Ports


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


toApi url =
    "https://www.dnd5eapi.co" ++ url


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( { monstersList = []
      , currentMonster =
            flags
                |> Maybe.andThen
                    (\v ->
                        case Json.Decode.decodeString monsterDecoder v of
                            Err _ ->
                                Nothing

                            Ok m ->
                                Just m
                    )
      , cachedMonstersString = flags
      , cachedMonsters = Dict.empty
      }
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
                    let
                        newCached =
                            Debug.log "NewLog" (Dict.insert monster.name monster model.cachedMonsters)
                    in
                    ( { model | currentMonster = Just monster, cachedMonsters = newCached }, saveData (Encode.encode 0 (monsterCacheEncode newCached)) )


type alias Model =
    { monstersList : List MonsterHeader
    , currentMonster : Maybe Monster
    , cachedMonstersString : Maybe String
    , cachedMonsters : Dict String Monster
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


monsterEncode : Monster -> Encode.Value
monsterEncode monster =
    Encode.object
        [ ( "name", Encode.string monster.name )
        , ( "size", Encode.string monster.size )
        ]


monsterCacheEncode monsters =
    Encode.dict identity monsterEncode monsters



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ p []
                [ text
                    (case model.cachedMonstersString of
                        Just x ->
                            x

                        Nothing ->
                            "<< NO CACHE >>"
                    )
                ]
            , case model.currentMonster of
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


viewMonster : Monster -> Html Msg
viewMonster monster =
    div []
        [ h2 [] [ text monster.name ]
        , Html.h6 [] [ text ("Size:" ++ monster.size) ]
        ]


saveData : String -> Cmd msg
saveData str =
    Ports.storeData str
