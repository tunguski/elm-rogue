module Main exposing (main)

{-| The app shell: a class-selection screen (with a persistent run-history table), then the game,
drawn through a selectable `Rogue.Render.Renderer`. A toolbar switches the active **mod** (`Ruleset`)
and **rendering engine**; nothing in `Rogue.Game` knows which is chosen — the point of the seams.

Finished runs (death or victory) are persisted to `localStorage` via `Storage` and reloaded on start,
so your last few delves survive a page reload.

Controls: arrows / WASD / HJKL move, Y U B N diagonals, `.` wait, `Z` search, `>` descend, `1`-`9`
use/equip an item, `R` restart.
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Mod.Default
import Mod.Hard
import Rogue.Content as Content exposing (ClassDef, Ruleset)
import Rogue.Game as Game exposing (Game)
import Rogue.Grid as Grid
import Rogue.Render exposing (Renderer)
import Rogue.Render.Ascii as AsciiRenderer
import Rogue.Render.Svg as SvgRenderer
import Set exposing (Set)
import Storage


type Screen
    = ClassSelect
    | Playing


type alias Model =
    { game : Game
    , screen : Screen
    , modName : String
    , rendererName : String
    , currentClass : String
    , history : List Run
    , showSheet : Bool
    , showBestiary : Bool
    , bestiary : Set String
    , targeting : Maybe Grid.Pos
    , damaged : Bool
    , seedInput : String
    , seedBump : Int
    }


{-| A persisted record of one finished delve. -}
type alias Run =
    { className : String
    , depth : Int
    , kills : Int
    , turns : Int
    , won : Bool
    }


type Msg
    = GameMsg Game.Msg
    | SelectMod String
    | SelectRenderer String
    | StartGame ClassDef
    | ToggleSheet
    | ToggleBestiary
    | SetSeed String
    | KeyPressed String
    | Loaded (Maybe String)


dailySeed : Int
dailySeed =
    20260610


startSeed : Int
startSeed =
    20260610


storageKey : String
storageKey =
    "elm-rogue-history"


maxHistory : Int
maxHistory =
    8


mods : List ( String, Ruleset )
mods =
    [ ( "Default", Mod.Default.ruleset )
    , ( "Hardcore", Mod.Hard.ruleset )
    ]


renderers : List ( String, Renderer Msg )
renderers =
    [ ( "SVG", SvgRenderer.renderer )
    , ( "ASCII", AsciiRenderer.renderer )
    ]


lookup : String -> List ( String, a ) -> Maybe a
lookup name pairs =
    case pairs of
        [] ->
            Nothing

        ( n, v ) :: rest ->
            if n == name then
                Just v

            else
                lookup name rest


rulesetNamed : String -> Ruleset
rulesetNamed name =
    lookup name mods |> Maybe.withDefault Mod.Default.ruleset


rendererNamed : String -> Renderer Msg
rendererNamed name =
    lookup name renderers |> Maybe.withDefault SvgRenderer.renderer


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.newGame Mod.Default.ruleset (Content.defaultClass Mod.Default.ruleset) startSeed
      , screen = ClassSelect
      , modName = "Default"
      , rendererName = "SVG"
      , currentClass = "Adventurer"
      , history = []
      , showSheet = False
      , showBestiary = False
      , bestiary = Set.empty
      , targeting = Nothing
      , damaged = False
      , seedInput = ""
      , seedBump = 0
      }
    , Storage.load storageKey Loaded
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded stored ->
            ( { model | history = parseHistory stored }, Cmd.none )

        ToggleSheet ->
            ( { model | showSheet = not model.showSheet }, Cmd.none )

        ToggleBestiary ->
            ( { model | showBestiary = not model.showBestiary }, Cmd.none )

        SetSeed text ->
            ( { model | seedInput = text }, Cmd.none )

        KeyPressed key ->
            handleKey (String.toLower key) model

        SelectRenderer name ->
            ( { model | rendererName = name }, Cmd.none )

        SelectMod name ->
            ( { model | modName = name, screen = ClassSelect }, Cmd.none )

        StartGame class ->
            ( { model
                | game = Game.newGame (rulesetNamed model.modName) class (chosenSeed model)
                , currentClass = class.name
                , bestiary = Set.empty
                , screen = Playing
              }
            , Cmd.none
            )

        GameMsg gm ->
            runGame gm model


{-| Apply a game message: handle restart (back to class select) or step the engine, folding the seen
monsters into the bestiary and persisting a finished run. Shared by `GameMsg` and key handling. -}
runGame : Game.Msg -> Model -> ( Model, Cmd Msg )
runGame gm model =
    case gm of
        Game.Restart ->
            ( { model
                | screen = ClassSelect
                , seedBump = model.seedBump + model.game.turn + model.game.depth * 1009 + 1
              }
            , Cmd.none
            )

        _ ->
            let
                nextGame =
                    Game.update gm model.game

                justEnded =
                    nextGame.gameOver && not model.game.gameOver

                bestiary =
                    Set.union model.bestiary (seenMonsters nextGame)

                base =
                    { model
                        | game = nextGame
                        , bestiary = bestiary
                        , damaged = nextGame.hero.hp < model.game.hero.hp
                    }
            in
            if justEnded then
                let
                    run =
                        { className = model.currentClass
                        , depth = nextGame.depth
                        , kills = nextGame.kills
                        , turns = nextGame.turn
                        , won = nextGame.won
                        }

                    history =
                        List.take maxHistory (run :: model.history)
                in
                ( { base | history = history }
                , Storage.save storageKey (encodeHistory history)
                )

            else
                ( base, Cmd.none )


{-| Route a keypress: overlays and the targeting cursor capture keys first; otherwise it's a normal
play key. Targeting (entered with `f`) cycles monsters with `Tab`, throws with `f`/Enter, cancels with
anything else. -}
handleKey : String -> Model -> ( Model, Cmd Msg )
handleKey key model =
    case model.targeting of
        Just target ->
            if key == "f" || key == "enter" || key == " " then
                runGame (Game.ThrowAt target) { model | targeting = Nothing }

            else if key == "tab" then
                ( { model | targeting = nextTarget model.game target }, Cmd.none )

            else
                ( { model | targeting = Nothing }, Cmd.none )

        Nothing ->
            case key of
                "i" ->
                    ( { model | showSheet = not model.showSheet }, Cmd.none )

                "m" ->
                    ( { model | showBestiary = not model.showBestiary }, Cmd.none )

                "f" ->
                    case nearestTarget model.game of
                        Just t ->
                            ( { model | targeting = Just t }, Cmd.none )

                        Nothing ->
                            runGame Game.Fire model

                _ ->
                    runGame (keyToGameMsg key) model


{-| The nearest visible monster to the hero, if any (the initial target). -}
nearestTarget : Game -> Maybe Grid.Pos
nearestTarget game =
    game.enemies
        |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
        |> List.foldl
            (\e best ->
                case best of
                    Nothing ->
                        Just e.pos

                    Just b ->
                        if Grid.chebyshev e.pos game.hero.pos < Grid.chebyshev b game.hero.pos then
                            Just e.pos

                        else
                            best
            )
            Nothing


{-| Cycle to the next visible monster after `current` (wraps), for Tab-targeting. -}
nextTarget : Game -> Grid.Pos -> Maybe Grid.Pos
nextTarget game current =
    let
        visible =
            game.enemies
                |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
                |> List.map .pos

        after =
            List.filter (\p -> p /= current) visible
    in
    case after of
        next :: _ ->
            Just next

        [] ->
            Just current



-- HISTORY PERSISTENCE ----------------------------------------------------------------------------


{-| Records are stored one per line as `class|depth|kills|turns|W|D` — a tiny delimited format, so no
JSON codecs are needed for this small amount of state. -}
encodeHistory : List Run -> String
encodeHistory runs =
    String.join "\n" (List.map encodeRun runs)


encodeRun : Run -> String
encodeRun run =
    String.join "|"
        [ run.className
        , String.fromInt run.depth
        , String.fromInt run.kills
        , String.fromInt run.turns
        , if run.won then
            "W"

          else
            "D"
        ]


parseHistory : Maybe String -> List Run
parseHistory stored =
    case stored of
        Nothing ->
            []

        Just text ->
            text
                |> String.split "\n"
                |> List.filterMap parseRun


parseRun : String -> Maybe Run
parseRun line =
    case String.split "|" line of
        [ name, d, k, t, result ] ->
            Maybe.map3
                (\depth kills turns ->
                    { className = name, depth = depth, kills = kills, turns = turns, won = result == "W" }
                )
                (String.toInt d)
                (String.toInt k)
                (String.toInt t)

        _ ->
            Nothing



-- VIEW -------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "font-family" "ui-monospace, Menlo, Consolas, monospace"
        , HA.style "background" "#0b0e14"
        , HA.style "color" "#c7d0dd"
        , HA.style "min-height" "100vh"
        , HA.style "padding" "16px 20px"
        , HA.style "box-sizing" "border-box"
        ]
        [ toolbar model
        , case model.screen of
            ClassSelect ->
                classSelectView model

            Playing ->
                Html.div []
                    [ Html.div
                        [ HA.style "text-align" "center", HA.style "font-size" "12px", HA.style "color" "#5b6b82", HA.style "margin" "10px 0 14px" ]
                        [ Html.text "move: WASD/HJKL · diag: Y U B N · wait: . · search: Z · aim: F (Tab cycle, F throw) · brew: C · inventory: I · bestiary: M · descend: > · use: 1-9 · restart: R" ]
                    , (rendererNamed model.rendererName).view (sceneFor model)
                    , if model.showSheet then
                        sheetView model.game

                      else
                        Html.text ""
                    , if model.showBestiary then
                        bestiaryView model

                      else
                        Html.text ""
                    ]
        ]


bestiaryView : Model -> Html Msg
bestiaryView model =
    let
        roster =
            (rulesetNamed model.modName).enemies ++ (rulesetNamed model.modName).bosses
    in
    Html.div
        [ HA.style "position" "fixed"
        , HA.style "inset" "0"
        , HA.style "background" "rgba(3,5,9,0.82)"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "z-index" "10"
        ]
        [ Html.div
            [ HA.style "background" "#0e131d"
            , HA.style "border" "1px solid #2a3550"
            , HA.style "border-radius" "14px"
            , HA.style "padding" "22px 26px"
            , HA.style "min-width" "440px"
            , HA.style "max-width" "560px"
            , HA.style "max-height" "80vh"
            , HA.style "overflow-y" "auto"
            , HA.style "color" "#c7d0dd"
            ]
            (Html.div [ HA.style "font-size" "18px", HA.style "font-weight" "700", HA.style "margin-bottom" "10px" ] [ Html.text "Bestiary" ]
                :: List.map (bestiaryRow model.bestiary) roster
                ++ [ Html.button
                        [ onClick ToggleBestiary
                        , HA.style "margin-top" "14px"
                        , HA.style "font" "inherit"
                        , HA.style "font-size" "12.5px"
                        , HA.style "cursor" "pointer"
                        , HA.style "color" "#c7d0dd"
                        , HA.style "background" "#161f38"
                        , HA.style "border" "1px solid #2a3550"
                        , HA.style "border-radius" "8px"
                        , HA.style "padding" "6px 14px"
                        ]
                        [ Html.text "Close (M)" ]
                   ]
            )
        ]


bestiaryRow : Set String -> Content.EnemyDef -> Html Msg
bestiaryRow seen def =
    if Set.member def.id seen then
        Html.div
            [ HA.style "display" "flex", HA.style "justify-content" "space-between", HA.style "font-size" "13px", HA.style "padding" "3px 0" ]
            [ Html.span [ HA.style "color" def.color ] [ Html.text (def.glyph ++ "  " ++ def.name) ]
            , Html.span [ HA.style "color" "#7f8ba0", HA.style "font-size" "11.5px" ]
                [ Html.text ("HP " ++ String.fromInt def.maxHp ++ " · DMG " ++ String.fromInt def.damage ++ " · DEF " ++ String.fromInt def.defense ++ " · " ++ String.fromInt def.xp ++ "xp") ]
            ]

    else
        Html.div [ HA.style "font-size" "13px", HA.style "padding" "3px 0", HA.style "color" "#3f4b5e" ]
            [ Html.text "????  — undiscovered" ]


sheetView : Game -> Html Msg
sheetView game =
    let
        hero =
            game.hero
    in
    Html.div
        [ HA.style "position" "fixed"
        , HA.style "inset" "0"
        , HA.style "background" "rgba(3,5,9,0.82)"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "z-index" "10"
        ]
        [ Html.div
            [ HA.style "background" "#0e131d"
            , HA.style "border" "1px solid #2a3550"
            , HA.style "border-radius" "14px"
            , HA.style "padding" "22px 26px"
            , HA.style "min-width" "420px"
            , HA.style "max-width" "560px"
            , HA.style "color" "#c7d0dd"
            ]
            [ Html.div [ HA.style "font-size" "18px", HA.style "font-weight" "700", HA.style "margin-bottom" "10px" ]
                [ Html.text "Character" ]
            , sheetStat "Level" (String.fromInt hero.level)
            , sheetStat "HP" (String.fromInt (max 0 hero.hp) ++ " / " ++ String.fromInt hero.maxHp)
            , sheetStat "Gold" (String.fromInt hero.gold)
            , Html.div [ HA.style "color" "#5b6b82", HA.style "font-size" "12px", HA.style "margin" "14px 0 6px" ]
                [ Html.text "Inventory (click to drop)" ]
            , if List.isEmpty hero.inventory then
                Html.div [ HA.style "color" "#3f4b5e", HA.style "font-size" "13px" ] [ Html.text "— empty —" ]

              else
                Html.div [] (List.indexedMap sheetItem hero.inventory)
            , Html.button
                [ onClick ToggleSheet
                , HA.style "margin-top" "14px"
                , HA.style "font" "inherit"
                , HA.style "font-size" "12.5px"
                , HA.style "cursor" "pointer"
                , HA.style "color" "#c7d0dd"
                , HA.style "background" "#161f38"
                , HA.style "border" "1px solid #2a3550"
                , HA.style "border-radius" "8px"
                , HA.style "padding" "6px 14px"
                ]
                [ Html.text "Close (I)" ]
            ]
        ]


sheetStat : String -> String -> Html Msg
sheetStat label value =
    Html.div [ HA.style "display" "flex", HA.style "justify-content" "space-between", HA.style "font-size" "13px", HA.style "padding" "2px 0" ]
        [ Html.span [ HA.style "color" "#7f8ba0" ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


sheetItem : Int -> Content.ItemDef -> Html Msg
sheetItem i def =
    Html.button
        [ onClick (GameMsg (Game.Drop i))
        , HA.style "display" "flex"
        , HA.style "justify-content" "space-between"
        , HA.style "width" "100%"
        , HA.style "text-align" "left"
        , HA.style "font" "inherit"
        , HA.style "font-size" "13px"
        , HA.style "color" "#c7d0dd"
        , HA.style "background" "#141b29"
        , HA.style "border" "1px solid #20304d"
        , HA.style "border-radius" "8px"
        , HA.style "padding" "6px 10px"
        , HA.style "margin-bottom" "4px"
        , HA.style "cursor" "pointer"
        ]
        [ Html.span [ HA.style "color" def.color ] [ Html.text (String.fromInt (i + 1) ++ ". " ++ def.name) ]
        , Html.span [ HA.style "color" "#7f8ba0", HA.style "font-size" "11.5px" ] [ Html.text (Content.describe def) ]
        ]


{-| The render scene for the current frame, with the targeting cursor overlaid when aiming. -}
sceneFor : Model -> Rogue.Render.Scene
sceneFor model =
    let
        scene =
            Game.toScene model.game
    in
    { scene | cursor = model.targeting, shake = model.damaged }


toolbar : Model -> Html Msg
toolbar model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "24px"
        , HA.style "justify-content" "center"
        , HA.style "align-items" "center"
        , HA.style "flex-wrap" "wrap"
        ]
        [ Html.div [ HA.style "font-size" "20px", HA.style "font-weight" "800", HA.style "letter-spacing" "1px" ] [ Html.text "elm-rogue" ]
        , chipGroup "Mod" (List.map Tuple.first mods) model.modName SelectMod
        , chipGroup "Renderer" (List.map Tuple.first renderers) model.rendererName SelectRenderer
        ]


chipGroup : String -> List String -> String -> (String -> Msg) -> Html Msg
chipGroup label names active toMsg =
    Html.div [ HA.style "display" "flex", HA.style "gap" "6px", HA.style "align-items" "center" ]
        (Html.span [ HA.style "color" "#5b6b82", HA.style "font-size" "12px" ] [ Html.text label ]
            :: List.map (\name -> chip name (name == active) (toMsg name)) names
        )


chip : String -> Bool -> Msg -> Html Msg
chip label active msg =
    Html.button
        [ onClick msg
        , HA.style "font" "inherit"
        , HA.style "font-size" "12.5px"
        , HA.style "cursor" "pointer"
        , HA.style "padding" "4px 10px"
        , HA.style "border-radius" "7px"
        , HA.style "border" "1px solid #2a3550"
        , HA.style "color"
            (if active then
                "#0b0e14"

             else
                "#c7d0dd"
            )
        , HA.style "background"
            (if active then
                "#7fae5a"

             else
                "#161f38"
            )
        ]
        [ Html.text label ]



-- CLASS SELECT -----------------------------------------------------------------------------------


chosenSeed : Model -> Int
chosenSeed model =
    case String.toInt (String.trim model.seedInput) of
        Just n ->
            n

        Nothing ->
            startSeed + model.seedBump


classSelectView : Model -> Html Msg
classSelectView model =
    Html.div
        [ HA.style "max-width" "820px"
        , HA.style "margin" "4vh auto"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "18px"
        ]
        [ Html.div [ HA.style "text-align" "center", HA.style "font-size" "16px", HA.style "color" "#9aa7ba" ]
            [ Html.text ("Choose your class — " ++ model.modName ++ " mod") ]
        , seedRow model
        , Html.div
            [ HA.style "display" "flex", HA.style "gap" "14px", HA.style "flex-wrap" "wrap", HA.style "justify-content" "center" ]
            (List.map classCard (rulesetNamed model.modName).classes)
        , historyView model.history
        ]


seedRow : Model -> Html Msg
seedRow model =
    Html.div
        [ HA.style "display" "flex", HA.style "gap" "8px", HA.style "justify-content" "center", HA.style "align-items" "center", HA.style "font-size" "12.5px" ]
        [ Html.span [ HA.style "color" "#5b6b82" ] [ Html.text "Seed" ]
        , Html.input
            [ HA.value model.seedInput
            , onInput SetSeed
            , HA.placeholder "random"
            , HA.style "font" "inherit"
            , HA.style "font-size" "12.5px"
            , HA.style "width" "130px"
            , HA.style "padding" "5px 9px"
            , HA.style "border-radius" "7px"
            , HA.style "border" "1px solid #2a3550"
            , HA.style "background" "#0e131d"
            , HA.style "color" "#c7d0dd"
            ]
            []
        , chip ("Daily " ++ String.fromInt dailySeed) (String.trim model.seedInput == String.fromInt dailySeed) (SetSeed (String.fromInt dailySeed))
        , chip "Clear" (model.seedInput == "") (SetSeed "")
        ]


classCard : ClassDef -> Html Msg
classCard class =
    Html.button
        [ onClick (StartGame class)
        , HA.style "width" "230px"
        , HA.style "text-align" "left"
        , HA.style "cursor" "pointer"
        , HA.style "font" "inherit"
        , HA.style "color" "#c7d0dd"
        , HA.style "background" "#11161f"
        , HA.style "border" "1px solid #2a3550"
        , HA.style "border-radius" "12px"
        , HA.style "padding" "16px 18px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "8px"
        ]
        [ Html.div [ HA.style "font-size" "18px", HA.style "font-weight" "700", HA.style "color" class.color ]
            [ Html.text (class.glyph ++ "  " ++ class.name) ]
        , Html.div [ HA.style "font-size" "12.5px", HA.style "color" "#9aa7ba", HA.style "line-height" "1.45", HA.style "min-height" "54px" ]
            [ Html.text class.description ]
        , Html.div [ HA.style "font-size" "12px", HA.style "color" "#7f8ba0" ]
            [ Html.text ("HP " ++ String.fromInt class.maxHp ++ " · DMG " ++ String.fromInt class.damage ++ " · DEF " ++ String.fromInt class.defense ++ " · FOV " ++ String.fromInt class.fovRadius) ]
        ]


historyView : List Run -> Html Msg
historyView runs =
    if List.isEmpty runs then
        Html.text ""

    else
        Html.div
            [ HA.style "max-width" "520px"
            , HA.style "margin" "8px auto 0"
            , HA.style "width" "100%"
            , HA.style "border-top" "1px solid #1b2433"
            , HA.style "padding-top" "12px"
            ]
            (Html.div [ HA.style "color" "#5b6b82", HA.style "font-size" "12px", HA.style "margin-bottom" "8px", HA.style "text-align" "center" ]
                [ Html.text "Recent delves (saved locally)" ]
                :: List.map runRow runs
            )


runRow : Run -> Html Msg
runRow run =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "justify-content" "space-between"
        , HA.style "font-size" "12.5px"
        , HA.style "padding" "3px 6px"
        , HA.style "color" "#9aa7ba"
        ]
        [ Html.span [ HA.style "color" (if run.won then "#5dd47a" else "#e0564b"), HA.style "min-width" "60px" ]
            [ Html.text (if run.won then "VICTORY" else "DIED") ]
        , Html.span [ HA.style "flex" "1", HA.style "padding-left" "8px" ] [ Html.text run.className ]
        , Html.span [] [ Html.text ("depth " ++ String.fromInt run.depth ++ " · " ++ String.fromInt run.kills ++ " slain · " ++ String.fromInt run.turns ++ "t") ]
        ]



-- INPUT ------------------------------------------------------------------------------------------


{-| Monster ids the hero can currently see — folded into the run's bestiary each step. -}
seenMonsters : Game -> Set String
seenMonsters game =
    game.enemies
        |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
        |> List.map (\e -> e.def.id)
        |> Set.fromList




keyToGameMsg : String -> Game.Msg
keyToGameMsg key =
    case String.toLower key of
        "arrowup" ->
            Game.Move Grid.dirN

        "arrowdown" ->
            Game.Move Grid.dirS

        "arrowleft" ->
            Game.Move Grid.dirW

        "arrowright" ->
            Game.Move Grid.dirE

        "w" ->
            Game.Move Grid.dirN

        "s" ->
            Game.Move Grid.dirS

        "a" ->
            Game.Move Grid.dirW

        "d" ->
            Game.Move Grid.dirE

        "k" ->
            Game.Move Grid.dirN

        "j" ->
            Game.Move Grid.dirS

        "h" ->
            Game.Move Grid.dirW

        "l" ->
            Game.Move Grid.dirE

        "y" ->
            Game.Move Grid.dirNW

        "u" ->
            Game.Move Grid.dirNE

        "b" ->
            Game.Move Grid.dirSW

        "n" ->
            Game.Move Grid.dirSE

        "." ->
            Game.Wait

        "z" ->
            Game.Search

        "f" ->
            Game.Fire

        "c" ->
            Game.Brew

        ">" ->
            Game.Descend

        "r" ->
            Game.Restart

        _ ->
            case String.toInt key of
                Just digit ->
                    if digit >= 1 && digit <= 9 then
                        Game.Use (digit - 1)

                    else
                        Game.NoOp

                Nothing ->
                    Game.NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
