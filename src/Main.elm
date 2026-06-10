module Main exposing (main)

{-| Milestones 2–3: generate a dungeon from a seed and draw it through the pluggable renderer seam.

`Main` owns only the wiring: it holds the generated level and a `Scene`, and delegates all drawing to
a `Rogue.Render.Renderer` (here the SVG one). The engine/content layers arrive in later milestones;
this stage proves the generator and the renderer boundary.
-}

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Rogue.Dungeon as Dungeon exposing (Generated)
import Rogue.Level as Level
import Rogue.Render as Render exposing (Scene)
import Rogue.Render.Svg as SvgRenderer
import Set exposing (Set)


type alias Model =
    { dungeon : Generated }


startSeed : Int
startSeed =
    20260610


init : Model
init =
    { dungeon = Dungeon.generate Dungeon.defaultConfig startSeed }


scene : Model -> Scene
scene model =
    let
        lvl =
            model.dungeon.level

        allCells =
            Set.fromList (List.map (\p -> ( p.x, p.y )) (Level.positions lvl))

        hud =
            Render.emptyHud
    in
    { level = lvl
    , visible = allCells
    , explored = allCells
    , glyphs = []
    , hud =
        { hud
            | title = "elm-rouge"
            , depth = 1
            , status = String.fromInt (List.length model.dungeon.rooms) ++ " rooms generated"
        }
    }


view : Model -> Html msg
view model =
    Html.div
        [ HA.style "font-family" "ui-monospace, Menlo, Consolas, monospace"
        , HA.style "background" "#0b0e14"
        , HA.style "color" "#c7d0dd"
        , HA.style "min-height" "100vh"
        , HA.style "padding" "24px"
        , HA.style "box-sizing" "border-box"
        ]
        [ Html.div [ HA.style "text-align" "center", HA.style "font-size" "12px", HA.style "color" "#5b6b82", HA.style "margin-bottom" "14px" ]
            [ Html.text "milestone 2–3 — seeded dungeon + pluggable SVG renderer" ]
        , SvgRenderer.renderer.view (scene model)
        ]


main : Program () Model msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }
