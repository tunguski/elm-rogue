module Main exposing (main)

{-| The app shell: a class-selection screen, then the game, drawn through a selectable
`Rogue.Render.Renderer`. A toolbar switches the active **mod** (`Ruleset`) and **rendering engine**;
nothing in `Rogue.Game` knows which is chosen — the whole point of the seams.

Controls: arrows / WASD / HJKL move, Y U B N diagonals, `.` wait, `>` descend, `1`-`9` use/equip an
item, `R` restart (re-pick class).
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Mod.Default
import Mod.Hard
import Rogue.Content as Content exposing (ClassDef, Ruleset)
import Rogue.Game as Game exposing (Game)
import Rogue.Grid as Grid
import Rogue.Render exposing (Renderer)
import Rogue.Render.Ascii as AsciiRenderer
import Rogue.Render.Svg as SvgRenderer


type Screen
    = ClassSelect
    | Playing


type alias Model =
    { game : Game
    , screen : Screen
    , modName : String
    , rendererName : String
    , seedBump : Int
    }


type Msg
    = GameMsg Game.Msg
    | SelectMod String
    | SelectRenderer String
    | StartGame ClassDef


startSeed : Int
startSeed =
    20260610


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


init : Model
init =
    { game = Game.newGame Mod.Default.ruleset (Content.defaultClass Mod.Default.ruleset) startSeed
    , screen = ClassSelect
    , modName = "Default"
    , rendererName = "SVG"
    , seedBump = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectRenderer name ->
            { model | rendererName = name }

        SelectMod name ->
            -- Classes differ per mod, so changing mod returns to the class picker.
            { model | modName = name, screen = ClassSelect }

        StartGame class ->
            { model
                | game = Game.newGame (rulesetNamed model.modName) class (startSeed + model.seedBump)
                , screen = ClassSelect
            }

        GameMsg Game.Restart ->
            { model
                | screen = ClassSelect
                , seedBump = model.seedBump + model.game.turn + model.game.depth * 1009 + 1
            }

        GameMsg gm ->
            { model | game = Game.update gm model.game }


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
                        [ Html.text "move: arrows / WASD / HJKL · diagonals: Y U B N · wait: . · search: Z · descend: > · use/equip: 1-9 · restart: R" ]
                    , (rendererNamed model.rendererName).view (Game.toScene model.game)
                    ]
        ]


toolbar : Model -> Html Msg
toolbar model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "24px"
        , HA.style "justify-content" "center"
        , HA.style "align-items" "center"
        , HA.style "flex-wrap" "wrap"
        ]
        [ Html.div [ HA.style "font-size" "20px", HA.style "font-weight" "800", HA.style "letter-spacing" "1px" ] [ Html.text "elm-rouge" ]
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


classSelectView : Model -> Html Msg
classSelectView model =
    let
        classes =
            (rulesetNamed model.modName).classes
    in
    Html.div
        [ HA.style "max-width" "780px"
        , HA.style "margin" "5vh auto"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "16px"
        ]
        [ Html.div [ HA.style "text-align" "center", HA.style "font-size" "16px", HA.style "color" "#9aa7ba" ]
            [ Html.text ("Choose your class — " ++ model.modName ++ " mod") ]
        , Html.div
            [ HA.style "display" "flex", HA.style "gap" "14px", HA.style "flex-wrap" "wrap", HA.style "justify-content" "center" ]
            (List.map classCard classes)
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


keyToMsg : String -> Msg
keyToMsg key =
    GameMsg (keyToGameMsg key)


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
    Browser.Events.onKeyDown (Decode.map keyToMsg (Decode.field "key" Decode.string))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }
