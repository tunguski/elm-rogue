module Main exposing (main)

{-| The app shell: holds a `Rogue.Game`, feeds it keyboard input, and draws it through whichever
`Rogue.Render.Renderer` is selected (the SVG one for now). All the game logic lives in `Rogue.Game`;
`Main` is only wiring — input in, `Scene` out.

Controls: arrows / WASD / HJKL to move, Y U B N for diagonals, `.` to wait, `>` to descend, `R` to
restart after death.
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as Decode
import Mod.Default
import Rogue.Game as Game exposing (Game)
import Rogue.Grid as Grid
import Rogue.Render.Svg as SvgRenderer


type alias Model =
    { game : Game }


startSeed : Int
startSeed =
    20260610


init : Model
init =
    { game = Game.newGame Mod.Default.ruleset startSeed }


update : Game.Msg -> Model -> Model
update msg model =
    case msg of
        Game.Restart ->
            -- The shell owns reseeding: derive a fresh dungeon from the finished run's progress.
            { model | game = Game.newGame Mod.Default.ruleset (startSeed + model.game.turn + model.game.depth * 1009 + 1) }

        _ ->
            { model | game = Game.update msg model.game }


view : Model -> Html Game.Msg
view model =
    Html.div
        [ HA.style "font-family" "ui-monospace, Menlo, Consolas, monospace"
        , HA.style "background" "#0b0e14"
        , HA.style "color" "#c7d0dd"
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "box-sizing" "border-box"
        ]
        [ Html.div
            [ HA.style "text-align" "center", HA.style "font-size" "12px", HA.style "color" "#5b6b82", HA.style "margin-bottom" "12px" ]
            [ Html.text "move: arrows / WASD / HJKL · diagonals: Y U B N · wait: . · descend: > · restart: R" ]
        , SvgRenderer.renderer.view (Game.toScene model.game)
        ]


keyToMsg : String -> Game.Msg
keyToMsg key =
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

        ">" ->
            Game.Descend

        "r" ->
            Game.Restart

        _ ->
            Game.NoOp


subscriptions : Model -> Sub Game.Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map keyToMsg (Decode.field "key" Decode.string))


main : Program () Model Game.Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }
