module Main exposing (main)

{-| The app shell: holds a `Rogue.Game`, feeds it keyboard input, and draws it through a *selectable*
`Rogue.Render.Renderer`. A toolbar switches both the active **mod** (a `Rogue.Content.Ruleset` — the
default bestiary or the Hardcore transform) and the active **rendering engine** (SVG tiles or ASCII)
at runtime. Nothing in `Rogue.Game` knows which is chosen: that is the whole point of the seams.

Controls: arrows / WASD / HJKL to move, Y U B N for diagonals, `.` wait, `>` descend, `1`-`9` use an
item, `R` restart.
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Mod.Default
import Mod.Hard
import Rogue.Content exposing (Ruleset)
import Rogue.Game as Game exposing (Game)
import Rogue.Grid as Grid
import Rogue.Render exposing (Renderer)
import Rogue.Render.Ascii as AsciiRenderer
import Rogue.Render.Svg as SvgRenderer


type alias Model =
    { game : Game
    , modName : String
    , rendererName : String
    , seedBump : Int
    }


type Msg
    = GameMsg Game.Msg
    | SelectMod String
    | SelectRenderer String


startSeed : Int
startSeed =
    20260610


{-| The installed mods — each is just a named `Ruleset`. Add a mod by adding a row here. -}
mods : List ( String, Ruleset )
mods =
    [ ( "Default", Mod.Default.ruleset )
    , ( "Hardcore", Mod.Hard.ruleset )
    ]


{-| The installed rendering engines — each a named `Renderer`. They consume the identical `Scene`. -}
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
    { game = Game.newGame Mod.Default.ruleset startSeed
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
            { model
                | modName = name
                , game = Game.newGame (rulesetNamed name) (startSeed + model.seedBump)
            }

        GameMsg Game.Restart ->
            let
                bump =
                    model.seedBump + model.game.turn + model.game.depth * 1009 + 1
            in
            { model | seedBump = bump, game = Game.newGame (rulesetNamed model.modName) (startSeed + bump) }

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
        , Html.div
            [ HA.style "text-align" "center", HA.style "font-size" "12px", HA.style "color" "#5b6b82", HA.style "margin" "10px 0 14px" ]
            [ Html.text "move: arrows / WASD / HJKL · diagonals: Y U B N · wait: . · descend: > · use item: 1-9 · restart: R" ]
        , (rendererNamed model.rendererName).view (Game.toScene model.game)
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
        , HA.style "color" (if active then "#0b0e14" else "#c7d0dd")
        , HA.style "background" (if active then "#7fae5a" else "#161f38")
        ]
        [ Html.text label ]


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
