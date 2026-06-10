module Rogue.Render.Svg exposing (renderer, cellSize)

{-| The default rendering engine: a crisp SVG view of the dungeon with fog-of-war dimming, glyphs for
the hero/monsters/items drawn as `<text>`, and an HTML heads-up display (HP bar, depth, turn, message
log) laid out beside the map.

It is a plain `Rogue.Render.Renderer` value, so the engine knows nothing about it — `Main` could
swap in `Rogue.Render.Ascii` (or a mod's renderer) and the game would play identically. All SVG tags
and attributes used here are within the elm-lang JS backend's runtime whitelist.
-}

import Html exposing (Html)
import Html.Attributes as HA
import Rogue.Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render exposing (Glyph, Hud, Renderer, Scene)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)
import Svg exposing (Svg, g, rect, svg, text_)
import Svg.Attributes as SA


cellSize : Int
cellSize =
    24


renderer : Renderer msg
renderer =
    { name = "SVG"
    , cellSize = cellSize
    , view = view
    }


view : Scene -> Html msg
view scene =
    let
        lvl =
            scene.level

        -- A viewport that scrolls with the hero: only the window of cells around the camera is drawn,
        -- so a huge map costs no more DOM nodes than a small one (the core culling optimisation).
        win =
            viewport scene.camera lvl.width lvl.height

        cells =
            List.concatMap
                (\vy -> List.map (\vx -> { x = vx, y = vy }) (List.range win.x0 win.x1))
                (List.range win.y0 win.y1)

        pxW =
            (win.x1 - win.x0 + 1) * cellSize

        pxH =
            (win.y1 - win.y0 + 1) * cellSize

        viewBoxStr =
            String.join " "
                [ String.fromInt (win.x0 * cellSize)
                , String.fromInt (win.y0 * cellSize)
                , String.fromInt pxW
                , String.fromInt pxH
                ]

        inWindow p =
            p.x >= win.x0 && p.x <= win.x1 && p.y >= win.y0 && p.y <= win.y1
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "18px"
        , HA.style "align-items" "flex-start"
        , HA.style "flex-wrap" "wrap"
        , HA.style "justify-content" "center"
        ]
        [ Html.div [ HA.style "position" "relative" ]
            [ svg
                [ SA.viewBox viewBoxStr
                , SA.width (String.fromInt pxW)
                , SA.height (String.fromInt pxH)
                , HA.style "background" "#05070b"
                , HA.style "border" "1px solid #1b2433"
                , HA.style "border-radius" "8px"
                , HA.style "max-width" "100%"
                , HA.style "height" "auto"
                ]
                [ g [] (List.filterMap (cellSvg scene) cells)
                , g [] (List.filterMap (glyphSvg scene) (List.sortBy .layer (List.filter (\gl -> inWindow gl.pos) scene.glyphs)))
                , g [] (List.map popupSvg (List.filter (\pp -> inWindow pp.pos) scene.popups))
                ]
            , overlayView scene.hud
            ]
        , Html.div [ HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "12px" ]
            [ minimapView scene
            , hudView scene.hud
            ]
        ]


{-| A tiny whole-floor minimap: explored cells as dots (walls vs open), the hero as a bright marker. -}
minimapView : Scene -> Html msg
minimapView scene =
    let
        lvl =
            scene.level

        scale =
            5

        -- Iterate only the explored cells (a Set), not the whole map, so the minimap's cost scales
        -- with what you've seen rather than the floor size.
        dot ( x, y ) =
            let
                tile =
                    Level.at { x = x, y = y } lvl
            in
            if tile == Empty then
                Nothing

            else
                Just
                    (rect
                        [ SA.x (px (x * scale))
                        , SA.y (px (y * scale))
                        , SA.width (px scale)
                        , SA.height (px scale)
                        , SA.fill (minimapColor tile)
                        ]
                        []
                    )

        heroDot =
            rect
                [ SA.x (px (scene.camera.x * scale))
                , SA.y (px (scene.camera.y * scale))
                , SA.width (px scale)
                , SA.height (px scale)
                , SA.fill "#ffe08a"
                ]
                []
    in
    Html.div []
        [ Html.div [ HA.style "color" "#5b6b82", HA.style "font-size" "11px", HA.style "margin-bottom" "4px" ] [ Html.text "Map" ]
        , svg
            [ SA.viewBox ("0 0 " ++ String.fromInt (lvl.width * scale) ++ " " ++ String.fromInt (lvl.height * scale))
            , SA.width (String.fromInt (lvl.width * scale))
            , SA.height (String.fromInt (lvl.height * scale))
            , HA.style "background" "#05070b"
            , HA.style "border" "1px solid #1b2433"
            , HA.style "border-radius" "4px"
            , HA.style "max-width" "100%"
            ]
            [ g [] (List.filterMap dot (Set.toList scene.explored)), heroDot ]
        ]


popupSvg : Rogue.Render.Popup -> Svg msg
popupSvg pp =
    text_
        [ SA.x (px (pp.pos.x * cellSize + cellSize // 2))
        , SA.y (px (pp.pos.y * cellSize - 2))
        , SA.fill pp.color
        , SA.fontSize (px (cellSize - 8))
        , SA.fontFamily "ui-monospace, Menlo, Consolas, monospace"
        , SA.textAnchor "middle"
        , SA.dominantBaseline "central"
        ]
        [ Svg.text pp.text ]


minimapColor : Tile -> String
minimapColor tile =
    case tile of
        Wall ->
            "#39455c"

        StairsDown ->
            "#d8b24c"

        StairsUp ->
            "#4f8bff"

        Water ->
            "#274b6b"

        Grass ->
            "#2f5a32"

        _ ->
            "#1a2434"


{-| The inclusive cell window to draw, centred on `camera` and clamped to the map. -}
viewport : Pos -> Int -> Int -> { x0 : Int, y0 : Int, x1 : Int, y1 : Int }
viewport camera width height =
    let
        vw =
            min width 31

        vh =
            min height 21

        clampStart c span extent =
            max 0 (min (extent - span) (c - span // 2))

        x0 =
            clampStart camera.x vw width

        y0 =
            clampStart camera.y vh height
    in
    { x0 = x0, y0 = y0, x1 = x0 + vw - 1, y1 = y0 + vh - 1 }


{-| A translucent end-of-game banner over the map: green for victory, red for death. -}
overlayView : Hud -> Html msg
overlayView hud =
    if not hud.gameOver then
        Html.text ""

    else
        let
            ( title, color ) =
                if hud.won then
                    ( "VICTORY", "#5dd47a" )

                else
                    ( "YOU DIED", "#e0564b" )
        in
        Html.div
            [ HA.style "position" "absolute"
            , HA.style "inset" "0"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "gap" "10px"
            , HA.style "background" "rgba(3,5,9,0.7)"
            , HA.style "border-radius" "8px"
            ]
            [ Html.div
                [ HA.style "font-size" "44px"
                , HA.style "font-weight" "800"
                , HA.style "letter-spacing" "4px"
                , HA.style "color" color
                ]
                [ Html.text title ]
            , Html.div [ HA.style "font-size" "14px", HA.style "color" "#c7d0dd" ]
                [ Html.text "press R to play again" ]
            ]



-- TERRAIN ----------------------------------------------------------------------------------------


{-| Draw one terrain cell — or nothing for an unseen cell, letting the SVG's own background show
through. Skipping unseen cells keeps the node count proportional to what's been explored, not to the
whole viewport (a real win on slow machines, where vDOM-diffing fewer nodes is cheaper). -}
cellSvg : Scene -> Pos -> Maybe (Svg msg)
cellSvg scene p =
    let
        vis =
            visibilityAt scene ( p.x, p.y )
    in
    case vis of
        Rogue.Render.Unseen ->
            Nothing

        _ ->
            let
                tile =
                    Level.at p scene.level

                dim =
                    vis == Rogue.Render.Remembered
            in
            Just
                (rect
                    [ SA.x (px (p.x * cellSize))
                    , SA.y (px (p.y * cellSize))
                    , SA.width (px cellSize)
                    , SA.height (px cellSize)
                    , SA.fill (tileColor scene.theme tile dim)
                    , SA.stroke "#070a10"
                    , SA.strokeWidth "1"
                    ]
                    []
                )


visibilityAt : Scene -> ( Int, Int ) -> Rogue.Render.Visibility
visibilityAt scene key =
    if Set.member key scene.visible then
        Rogue.Render.Visible

    else if Set.member key scene.explored then
        Rogue.Render.Remembered

    else
        Rogue.Render.Unseen


tileColor : Rogue.Render.Theme -> Tile -> Bool -> String
tileColor theme tile dim =
    let
        ( lit, faded ) =
            case tile of
                Wall ->
                    ( theme.wallLit, theme.wallDim )

                Floor ->
                    ( theme.floorLit, theme.floorDim )

                Door ->
                    ( theme.door, "#3c2a18" )

                OpenDoor ->
                    ( theme.floorLit, theme.floorDim )

                LockedDoor ->
                    ( "#c9a23a", "#5a4a1c" )

                SecretDoor ->
                    -- Disguised as a wall until found.
                    ( theme.wallLit, theme.wallDim )

                StairsDown ->
                    ( "#d8b24c", "#5a4a20" )

                StairsUp ->
                    ( "#4f8bff", "#23365c" )

                Chasm ->
                    ( "#04050a", "#04050a" )

                Water ->
                    ( "#274b6b", "#16293a" )

                Grass ->
                    ( "#2f5a32", "#1a3019" )

                Empty ->
                    ( "#05070b", "#05070b" )
    in
    if dim then
        faded

    else
        lit



-- GLYPHS -----------------------------------------------------------------------------------------


glyphSvg : Scene -> Glyph -> Maybe (Svg msg)
glyphSvg scene glyph =
    let
        key =
            ( glyph.pos.x, glyph.pos.y )

        seen =
            Set.member key scene.visible
    in
    if not seen then
        Nothing

    else
        Just
            (text_
                [ SA.x (px (glyph.pos.x * cellSize + cellSize // 2))
                , SA.y (px (glyph.pos.y * cellSize + cellSize // 2))
                , SA.fill glyph.color
                , SA.fontSize (px (cellSize - 6))
                , SA.fontFamily "ui-monospace, Menlo, Consolas, monospace"
                , SA.textAnchor "middle"
                , SA.dominantBaseline "central"
                ]
                [ Svg.text glyph.char ]
            )



-- HUD --------------------------------------------------------------------------------------------


hudView : Hud -> Html msg
hudView hud =
    Html.div
        [ HA.style "min-width" "210px"
        , HA.style "max-width" "260px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "12px"
        , HA.style "color" "#c7d0dd"
        , HA.style "font-family" "ui-monospace, Menlo, Consolas, monospace"
        ]
        [ Html.div [ HA.style "font-size" "20px", HA.style "font-weight" "700" ] [ Html.text hud.title ]
        , statLine "Depth" (String.fromInt hud.depth ++ "  " ++ hud.region)
        , statLine "Level" (String.fromInt hud.level ++ "  (xp " ++ String.fromInt hud.xp ++ "/" ++ String.fromInt hud.xpNext ++ ")")
        , statLine "Turn" (String.fromInt hud.turn)
        , statLine "Gold" (String.fromInt hud.gold)
        , if hud.hunger /= "" then
            statLine "Hunger" hud.hunger

          else
            Html.text ""
        , hpBar hud
        , statLine "Weapon" hud.weapon
        , statLine "Armour" hud.armour
        , if List.isEmpty hud.statuses then
            Html.text ""

          else
            Html.div [ HA.style "font-size" "12.5px", HA.style "color" "#8fd14f" ]
                [ Html.text ("Status: " ++ String.join ", " hud.statuses) ]
        , if hud.status /= "" then
            Html.div [ HA.style "color" "#f0c674", HA.style "font-size" "13px" ] [ Html.text hud.status ]

          else
            Html.text ""
        , inventoryView hud.inventory
        , logView hud.log
        ]


inventoryView : List String -> Html msg
inventoryView items =
    Html.div
        [ HA.style "border-top" "1px solid #1b2433"
        , HA.style "padding-top" "8px"
        , HA.style "font-size" "12.5px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "2px"
        ]
        (Html.div [ HA.style "color" "#5b6b82", HA.style "margin-bottom" "2px" ] [ Html.text "Inventory (1-9 to use)" ]
            :: (if List.isEmpty items then
                    [ Html.div [ HA.style "color" "#3f4b5e" ] [ Html.text "— empty —" ] ]

                else
                    List.indexedMap
                        (\i name ->
                            Html.div []
                                [ Html.span [ HA.style "color" "#7f8ba0" ] [ Html.text (String.fromInt (i + 1) ++ ". ") ]
                                , Html.text name
                                ]
                        )
                        (List.take 9 items)
               )
        )


statLine : String -> String -> Html msg
statLine label value =
    Html.div [ HA.style "display" "flex", HA.style "justify-content" "space-between", HA.style "font-size" "13px" ]
        [ Html.span [ HA.style "color" "#5b6b82" ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


hpBar : Hud -> Html msg
hpBar hud =
    let
        frac =
            if hud.maxHp <= 0 then
                0

            else
                toFloat (max 0 hud.hp) / toFloat hud.maxHp

        pct =
            String.fromInt (round (frac * 100)) ++ "%"

        color =
            if frac > 0.5 then
                "#5dd47a"

            else if frac > 0.25 then
                "#e0b341"

            else
                "#e0564b"
    in
    Html.div []
        [ statLine "HP" (String.fromInt (max 0 hud.hp) ++ " / " ++ String.fromInt hud.maxHp)
        , Html.div
            [ HA.style "height" "10px"
            , HA.style "background" "#10151f"
            , HA.style "border-radius" "5px"
            , HA.style "overflow" "hidden"
            , HA.style "margin-top" "4px"
            ]
            [ Html.div
                [ HA.style "height" "100%"
                , HA.style "width" pct
                , HA.style "background" color
                ]
                []
            ]
        ]


logView : List String -> Html msg
logView entries =
    Html.div
        [ HA.style "margin-top" "6px"
        , HA.style "border-top" "1px solid #1b2433"
        , HA.style "padding-top" "8px"
        , HA.style "font-size" "12.5px"
        , HA.style "line-height" "1.5"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "2px"
        , HA.style "min-height" "90px"
        ]
        (List.map (\e -> Html.div [ HA.style "color" "#9aa7ba" ] [ Html.text e ]) entries)


px : Int -> String
px n =
    String.fromInt n
