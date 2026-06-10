module Rogue.Render.Ascii exposing (renderer)

{-| A second, completely independent rendering engine: a classic text-mode roguelike view built from
coloured monospace cells, with a plain-text HUD.

It consumes the very same `Rogue.Render.Scene` the SVG renderer does and is a `Renderer` value just
like it — so `Main` swaps between them at runtime with no change to the engine. This is the concrete
proof of the "alternative game rendering engine" promise: the simulation has no idea whether it is
being drawn as SVG tiles or ASCII glyphs.
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Rogue.Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render exposing (Glyph, Hud, Renderer, Scene)
import Rogue.Tile as Tile exposing (Tile)
import Set exposing (Set)


renderer : Renderer msg
renderer =
    { name = "ASCII"
    , cellSize = 0
    , view = view
    }


view : Scene -> Html msg
view scene =
    renderWith scene


{-| Pre-index the topmost glyph per cell so each cell is one dictionary lookup. -}
topGlyphs : Scene -> Dict ( Int, Int ) Glyph
topGlyphs scene =
    List.foldl
        (\gph acc ->
            let
                key =
                    ( gph.pos.x, gph.pos.y )
            in
            case Dict.get key acc of
                Just existing ->
                    if gph.layer >= existing.layer then
                        Dict.insert key gph acc

                    else
                        acc

                Nothing ->
                    Dict.insert key gph acc
        )
        Dict.empty
        scene.glyphs


renderWith : Scene -> Html msg
renderWith scene =
    let
        glyphs =
            topGlyphs scene
    in
    Html.div [ HA.class "rg-gamewrap" ]
        [ Html.div [ HA.class "rg-ascii-map" ]
            (let
                win =
                    asciiWindow scene.camera scene.level.width scene.level.height
             in
             List.map (\y -> rowView scene glyphs win y) (List.range win.y0 win.y1)
                ++ [ overlayView scene.hud ]
            )
        , hudView scene.hud
        ]


{-| A camera-centred window of cells to render — the ASCII analogue of the SVG viewport, so a big map
costs no more `<span>`s than a small one. -}
asciiWindow : Pos -> Int -> Int -> { x0 : Int, y0 : Int, x1 : Int, y1 : Int }
asciiWindow camera width height =
    let
        vw =
            min width 41

        vh =
            min height 23

        start c span extent =
            max 0 (min (extent - span) (c - span // 2))

        x0 =
            start camera.x vw width

        y0 =
            start camera.y vh height
    in
    { x0 = x0, y0 = y0, x1 = x0 + vw - 1, y1 = y0 + vh - 1 }


rowView : Scene -> Dict ( Int, Int ) Glyph -> { x0 : Int, y0 : Int, x1 : Int, y1 : Int } -> Int -> Html msg
rowView scene glyphs win y =
    Html.div [ HA.class "rg-ascii-row" ]
        (List.map (\x -> cellView scene glyphs { x = x, y = y }) (List.range win.x0 win.x1))


cellView : Scene -> Dict ( Int, Int ) Glyph -> Pos -> Html msg
cellView scene glyphs p =
    let
        key =
            ( p.x, p.y )

        visible =
            Set.member key scene.visible

        explored =
            Set.member key scene.explored

        ( ch, color ) =
            if visible then
                case Dict.get key glyphs of
                    Just gph ->
                        ( gph.char, gph.color )

                    Nothing ->
                        ( Tile.glyph (Level.at p scene.level), tileColor scene.theme (Level.at p scene.level) False )

            else if explored then
                ( Tile.glyph (Level.at p scene.level), tileColor scene.theme (Level.at p scene.level) True )

            else
                ( " ", "#05070b" )
    in
    Html.span
        [ HA.class "rg-ascii-cell"
        , HA.style "color" color
        ]
        [ Html.text ch ]


tileColor : Rogue.Render.Theme -> Tile -> Bool -> String
tileColor theme tile dim =
    let
        lit =
            case tile of
                Tile.Wall ->
                    theme.wallLit

                Tile.Floor ->
                    theme.floorLit

                Tile.Door ->
                    theme.door

                Tile.OpenDoor ->
                    theme.floorLit

                Tile.SecretDoor ->
                    theme.wallLit

                Tile.StairsDown ->
                    "#d8b24c"

                Tile.StairsUp ->
                    "#4f8bff"

                Tile.Water ->
                    "#3a6e96"

                Tile.Grass ->
                    "#4a8a4d"

                _ ->
                    "#1b2433"
    in
    if dim then
        theme.wallDim

    else
        lit



-- HUD --------------------------------------------------------------------------------------------


hudView : Hud -> Html msg
hudView hud =
    Html.div [ HA.class "rg-hud rg-hud--ascii" ]
        ([ Html.div [ HA.class "rg-hud-title" ] [ Html.text hud.title ]
         , line ("Depth " ++ String.fromInt hud.depth ++ " " ++ hud.region ++ "   Turn " ++ String.fromInt hud.turn)
         , line ("Level " ++ String.fromInt hud.level ++ "  xp " ++ String.fromInt hud.xp ++ "/" ++ String.fromInt hud.xpNext)
         , line ("HP " ++ String.fromInt (max 0 hud.hp) ++ "/" ++ String.fromInt hud.maxHp ++ "   Gold " ++ String.fromInt hud.gold ++ (if hud.hunger /= "" then "   " ++ hud.hunger else ""))
         , line ("Wpn " ++ hud.weapon)
         , line ("Arm " ++ hud.armour)
         , if hud.ring /= "" then
            line ("Rng " ++ hud.ring)

           else
            Html.text ""
         , if List.isEmpty hud.statuses then
            Html.text ""

           else
            Html.div [ HA.class "rg-status-good" ] [ Html.text ("Status: " ++ String.join ", " hud.statuses) ]
         , if hud.status /= "" then
            Html.div [ HA.class "rg-status-note" ] [ Html.text hud.status ]

           else
            Html.text ""
         , Html.div [ HA.class "rg-inv-label", HA.style "margin-top" "4px" ] [ Html.text "Inventory (1-9):" ]
         ]
            ++ inventoryLines hud.inventory
            ++ [ Html.div [ HA.class "rg-inv-label", HA.style "margin-top" "6px" ] [ Html.text "Log:" ] ]
            ++ List.map (\e -> Html.div [ HA.class "rg-log-entry" ] [ Html.text e ]) hud.log
        )


inventoryLines : List String -> List (Html msg)
inventoryLines items =
    if List.isEmpty items then
        [ Html.div [ HA.class "rg-inv-empty" ] [ Html.text "  — empty —" ] ]

    else
        List.indexedMap
            (\i name -> Html.div [] [ Html.text ("  " ++ String.fromInt (i + 1) ++ ". " ++ name) ])
            (List.take 9 items)


line : String -> Html msg
line s =
    Html.div [] [ Html.text s ]


overlayView : Hud -> Html msg
overlayView hud =
    if not hud.gameOver then
        Html.text ""

    else
        let
            ( title, color ) =
                if hud.won then
                    ( "*** VICTORY ***", "#5dd47a" )

                else
                    ( "*** YOU DIED ***", "#e0564b" )
        in
        Html.div
            [ HA.class "rg-banner"
            , HA.style "color" color
            , HA.style "font-size" "26px"
            , HA.style "font-weight" "800"
            , HA.style "letter-spacing" "3px"
            ]
            [ Html.text title ]
