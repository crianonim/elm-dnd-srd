module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, h2, hr, input, label, p, span, text)
import Html.Attributes exposing (checked, class, disabled, for, id, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Encode as Encode
import Platform.Cmd exposing (Cmd)
import Ports
import Random
import Set exposing (Set)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


toApi : String -> String
toApi url =
    "https://www.dnd5eapi.co" ++ url


init : String -> ( Model, Cmd Msg )
init flags =
    ( { monstersList = []
      , currentMonster = Nothing
      , cachedMonstersString = Nothing
      , cachedMonsters = Dict.empty
      , rnd = []
      , favouriteMonsters = Set.fromList (Result.withDefault [] (Json.Decode.decodeString (Json.Decode.list string) flags))
      , filterFavourite = False
      , filterString = ""
      , arenaHome = []
      , arenaAway = []
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
            case Debug.log "RESULT" result of
                Err x ->
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
                Err _ ->
                    ( model, Cmd.none )

                Ok monster ->
                    let
                        newCached =
                            Debug.log "NewLog" (Dict.insert monster.name monster model.cachedMonsters)
                    in
                    ( { model | currentMonster = Just monster, cachedMonsters = newCached }, Cmd.none )

        InitRandom ->
            ( model, Random.generate GotRandom (Random.list 10000 (Random.float 0 1)) )

        GotRandom list ->
            ( { model | rnd = list }, Cmd.none )

        ToggleMonsterFave index isFavorited ->
            let
                updatedFavouriteMonsters =
                    if isFavorited then
                        Set.remove index model.favouriteMonsters

                    else
                        Set.insert index model.favouriteMonsters
            in
            ( { model
                | favouriteMonsters = updatedFavouriteMonsters
              }
            , saveData (Encode.encode 0 (monsterFaveEncode updatedFavouriteMonsters))
            )

        ToggleFilterFavorite ->
            ( { model | filterFavourite = not model.filterFavourite }, Cmd.none )

        UpdateFilter filter ->
            ( { model | filterString = filter }, Cmd.none )

        AddToArena team ->
            case model.currentMonster of
                Nothing ->
                    ( model, Cmd.none )

                Just monster ->
                    case team of
                        Home ->
                            ( { model | arenaHome = model.arenaHome ++ [ monster ] }, Cmd.none )

                        Away ->
                            ( { model | arenaAway = model.arenaAway ++ [ monster ] }, Cmd.none )


type alias Model =
    { monstersList : List MonsterHeader
    , currentMonster : Maybe Monster
    , cachedMonstersString : Maybe String
    , cachedMonsters : Dict String Monster
    , rnd : List Float
    , favouriteMonsters : Set String
    , filterFavourite : Bool
    , filterString : String
    , arenaHome : List Monster
    , arenaAway : List Monster
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
    | ToggleMonsterFave String Bool
    | ToggleFilterFavorite
    | UpdateFilter String
    | AddToArena Team


type Team
    = Home
    | Away


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


monsterCacheEncode : Dict String Monster -> Encode.Value
monsterCacheEncode monsters =
    Encode.dict identity monsterEncode monsters


monsterFaveEncode : Set String -> Encode.Value
monsterFaveEncode faves =
    Encode.set Encode.string faves


filterMonsterList : Model -> List MonsterHeader
filterMonsterList model =
    List.filter
        (\m ->
            String.contains (String.toLower model.filterString) (String.toLower m.index)
                && (if model.filterFavourite then
                        Set.member m.index model.favouriteMonsters

                    else
                        True
                   )
        )
        model.monstersList



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [ class "monster-browser" ]
                [ div [ class "monster-list" ]
                    [ viewFilterMonster model
                    , div [] (List.map (\m -> viewMonsterHeader (Set.member m.index model.favouriteMonsters) m) (filterMonsterList model))
                    ]
                , div [ class "current-monster" ]
                    [ case model.currentMonster of
                        Nothing ->
                            text "No monsters loaded..."

                        Just monster ->
                            viewMonster monster
                    ]
                , viewArenaSelection model
                ]
            ]
        ]


viewFilterMonster : { a | filterFavourite : Bool, filterString : String } -> Html Msg
viewFilterMonster { filterFavourite, filterString } =
    div [ class "filter" ]
        [ span [ onClick ToggleFilterFavorite, class "star" ]
            [ text
                (if filterFavourite then
                    "★"

                 else
                    "☆"
                )
            ]
        , input [ id "filter-string", value filterString, type_ "search", onInput UpdateFilter ]
            []
        ]


viewMonsterHeader : Bool -> MonsterHeader -> Html Msg
viewMonsterHeader isFavorited { index, name, url } =
    div [ title index, class "monster-header" ]
        [ span [ onClick (ToggleMonsterFave index isFavorited), class "star" ]
            [ text
                (if isFavorited then
                    "★"

                 else
                    "☆"
                )
            ]
        , div
            [ onClick (GetMonster url) ]
            [ text name ]
        ]


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
    div [ class "action" ]
        [ p [ class "action-name" ] [ text action.name ]
        , p [ class "action-description" ] [ text action.desc ]
        , div [] (List.map viewDamage action.damage)
        , p [ class "action-attack-bonus" ]
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
    div [ class "abilities" ]
        [ div []
            [ div [] [ text "Str" ]
            , div [] [ text (String.fromInt abilities.strength) ]
            ]
        , div []
            [ div [] [ text "Dex" ]
            , div [] [ text (String.fromInt abilities.dexterity) ]
            ]
        , div []
            [ div [] [ text "Con" ]
            , div [] [ text (String.fromInt abilities.constitution) ]
            ]
        , div []
            [ div [] [ text "Int" ]
            , div [] [ text (String.fromInt abilities.intelligence) ]
            ]
        , div []
            [ div [] [ text "Wis" ]
            , div [] [ text (String.fromInt abilities.wisdom) ]
            ]
        , div []
            [ div [] [ text "Cha" ]
            , div [] [ text (String.fromInt abilities.charisma) ]
            ]
        ]


viewArenaSelection : Model -> Html Msg
viewArenaSelection model =
    div [ class "arena-selection" ]
        [ h2 [] [ text "Arena Selection" ]
        , div [ class "arena-teams" ]
            [ viewArenaTeam Home model.arenaHome (maybeToBool model.currentMonster)
            , viewArenaTeam Away model.arenaAway (maybeToBool model.currentMonster)
            ]
        ]


viewArenaTeam : Team -> List Monster -> Bool -> Html Msg
viewArenaTeam team monsters canAdd =
    div [ class "arena-team" ]
        [ button [ onClick (AddToArena team), disabled (not canAdd) ]
            [ text "Add to Team!" ]
        , div
            []
            (List.map
                (\m -> text m.name)
                monsters
            )
        ]


saveData : String -> Cmd msg
saveData str =
    Ports.storeData str


maybeToBool : Maybe a -> Bool
maybeToBool m =
    case m of
        Just _ ->
            True

        Nothing ->
            False
