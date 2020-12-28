module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, hr, p, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import Ports
import Random


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
      , rnd = []
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

        InitRandom ->
            ( model, Random.generate GotRandom (Random.list 10000 (Random.float 0 1)) )

        GotRandom list ->
            ( { model | rnd = list }, Cmd.none )


type alias Model =
    { monstersList : List MonsterHeader
    , currentMonster : Maybe Monster
    , cachedMonstersString : Maybe String
    , cachedMonsters : Dict String Monster
    , rnd : List Float
    }


type alias MonsterHeader =
    { index : String
    , name : String
    , url : String
    }


type alias Monster =
    { name : String
    , size : String
    , abilities : Abilities
    , alignment : String
    , armorClass : Int
    , hitPoints : Int
    , actions : List MonsterAction
    }


type alias Abilities =
    { strength : Int
    , dexterity : Int
    , constitution : Int
    , intelligence : Int
    , wisdom : Int
    , charisma : Int
    }


type alias DamageType =
    { index : String
    }


type alias ActionDamage =
    { damageType : DamageType
    , damageDice : String
    }


type alias MonsterAction =
    { name : String
    , desc : String
    , attackBonus : Maybe Int
    , damage : List ActionDamage
    }


type Msg
    = GotMonstersList (Result Http.Error (List MonsterHeader))
    | GetMonster String
    | GotMonster (Result Http.Error Monster)
    | InitRandom
    | GotRandom (List Float)


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
    Json.Decode.map7 Monster
        (field "name" string)
        (field "size" string)
        monsterAbilitiesDecoder
        (field "alignment" string)
        (field "armor_class" int)
        (field "hit_points" int)
        (field "actions" (list actionDecoder))


monsterAbilitiesDecoder : Decoder Abilities
monsterAbilitiesDecoder =
    Json.Decode.map6 Abilities
        (field "strength" int)
        (field "dexterity" int)
        (field "constitution" int)
        (field "intelligence" int)
        (field "wisdom" int)
        (field "charisma" int)


actionDecoder : Decoder MonsterAction
actionDecoder =
    Json.Decode.map4 MonsterAction
        (field "name" string)
        (field "desc" string)
        (Json.Decode.maybe (field "attack_bonus" int))
        (field "damage" (list actionDamageDecoder))


actionDamageDecoder : Decoder ActionDamage
actionDamageDecoder =
    Json.Decode.map2 ActionDamage
        (field "damage_type" damageTypeDecoder)
        (field "damage_dice" string)


damageTypeDecoder : Decoder DamageType
damageTypeDecoder =
    Json.Decode.map DamageType (field "index" string)


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
            [ button [ onClick InitRandom ] [ text "Init rnd" ]
            , p []
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
    p [ title index, onClick (GetMonster url) ] [ text name ]


viewMonster : Monster -> Html Msg
viewMonster monster =
    div [ class "monster" ]
        [ p [] [ text monster.name ]
        , p [] [ text ("Size:" ++ monster.size) ]
        , p [] [ text monster.alignment ]
        , hr [] []
        , p [] [ text ("Armor Class: " ++ String.fromInt monster.armorClass) ]
        , p [] [ text ("Hit Points: " ++ String.fromInt monster.hitPoints) ]
        , hr [] []
        , div [] [ viewMonsterAbilities monster.abilities ]
        , div [] [ viewMonsterActions monster.actions ]
        ]


viewMonsterActions : List MonsterAction -> Html msg
viewMonsterActions actions =
    div [] (List.map viewMonsterAction actions)


viewMonsterAction : MonsterAction -> Html msg
viewMonsterAction action =
    div []
        [ p [] [ text action.name ]
        , p [] [ text action.desc ]
        , div [] (List.map viewDamage action.damage)
        , p []
            [ case action.attackBonus of
                Just attackBonus ->
                    text ("Attack Bonus: " ++ String.fromInt attackBonus)

                Nothing ->
                    text ""
            ]
        ]


viewDamage : ActionDamage -> Html msg
viewDamage damage =
    " ( " ++ damage.damageDice ++ " of " ++ damage.damageType.index ++ " )" |> text


viewMonsterAbilities : Abilities -> Html msg
viewMonsterAbilities abilities =
    div []
        [ div []
            [ text "Str"
            , text (String.fromInt abilities.strength)
            ]
        , div []
            [ text "Dex"
            , text (String.fromInt abilities.dexterity)
            ]
        , div []
            [ text "Con"
            , text (String.fromInt abilities.constitution)
            ]
        , div []
            [ text "Int"
            , text (String.fromInt abilities.intelligence)
            ]
        , div []
            [ text "Wis"
            , text (String.fromInt abilities.wisdom)
            ]
        , div []
            [ text "Cha"
            , text (String.fromInt abilities.charisma)
            ]
        ]


saveData : String -> Cmd msg
saveData str =
    Ports.storeData str
