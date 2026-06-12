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
    Html.div [ HA.class "rg-gamewrap" ]
        [ Html.div
            [ HA.class
                (if scene.shake then
                    "rg-mapwrap rg-shake-on"

                 else
                    "rg-mapwrap"
                )
            ]
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
                , g [] (List.map gasSvg (List.filter (\gc -> inWindow gc.pos) scene.gas))
                , g [] (List.filterMap (glyphSvg scene) (List.sortBy .layer (List.filter (\gl -> inWindow gl.pos) scene.glyphs)))
                , g [] (List.map popupSvg (List.filter (\pp -> inWindow pp.pos) scene.popups))
                , cursorSvg scene.cursor
                ]
            , lowHpVignette scene.hud
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

        -- Adapt the minimap cell size to the floor so a large map still fits the HUD column crisply.
        scale =
            if lvl.width > 56 then
                3

            else
                4

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
        [ Html.div [ HA.class "rg-minimap-label" ] [ Html.text "Map" ]
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
        , HA.style "animation" "rg-float 0.7s ease-out forwards"
        ]
        [ Svg.text pp.text ]


{-| A pulsing red edge-glow overlay when the hero is critically wounded (HP at or below a quarter).
The keyframes/styling live in app.css; this just toggles the `.rg-vignette` element on. -}
lowHpVignette : Rogue.Render.Hud -> Html.Html msg
lowHpVignette hud =
    if hud.maxHp > 0 && hud.hp > 0 && hud.hp * 4 <= hud.maxHp then
        Html.div [ HA.class "rg-vignette" ] []

    else
        Html.text ""


gasSvg : Rogue.Render.GasCell -> Svg msg
gasSvg gc =
    rect
        [ SA.x (px (gc.pos.x * cellSize))
        , SA.y (px (gc.pos.y * cellSize))
        , SA.width (px cellSize)
        , SA.height (px cellSize)
        , SA.fill gc.color
        , HA.style "opacity" (String.fromFloat gc.alpha)
        ]
        []


cursorSvg : Maybe Pos -> Svg msg
cursorSvg maybeCursor =
    case maybeCursor of
        Nothing ->
            g [] []

        Just c ->
            rect
                [ SA.x (px (c.x * cellSize + 1))
                , SA.y (px (c.y * cellSize + 1))
                , SA.width (px (cellSize - 2))
                , SA.height (px (cellSize - 2))
                , SA.fill "none"
                , SA.stroke "#ff5a5a"
                , SA.strokeWidth "2"
                , SA.rx "2"
                , HA.style "animation" "rg-pulse 0.9s ease-in-out infinite"
                ]
                []


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
        Html.div [ HA.class "rg-banner" ]
            [ Html.div [ HA.class "rg-banner-title", HA.style "color" color ]
                [ Html.text title ]
            , Html.div [ HA.class "rg-banner-sub" ]
                [ Html.text ("Depth " ++ String.fromInt hud.depth ++ " " ++ hud.region ++ " · level " ++ String.fromInt hud.level) ]
            , Html.div [ HA.class "rg-banner-sub" ]
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
                    ( "#140d20", "#0a0712" )

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
    Html.div [ HA.class "rg-hud" ]
        [ Html.div [ HA.class "rg-hud-title" ] [ Html.text hud.title ]
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
        , if hud.ring /= "" then
            statLine "Ring" hud.ring

          else
            Html.text ""
        , if hud.ability /= "" then
            statLine "Ability" hud.ability

          else
            Html.text ""
        , if List.isEmpty hud.statuses then
            Html.text ""

          else
            Html.div [ HA.class "rg-status-good" ]
                [ Html.text ("Status: " ++ String.join ", " hud.statuses) ]
        , if hud.status /= "" then
            Html.div [ HA.class "rg-status-note" ] [ Html.text hud.status ]

          else
            Html.text ""
        , inventoryView hud.inventory
        , logView hud.log
        ]


inventoryView : List String -> Html msg
inventoryView items =
    Html.div [ HA.class "rg-inv" ]
        (Html.div [ HA.class "rg-inv-label" ] [ Html.text "Inventory (1-9 to use)" ]
            :: (if List.isEmpty items then
                    [ Html.div [ HA.class "rg-inv-empty" ] [ Html.text "— empty —" ] ]

                else
                    List.indexedMap
                        (\i name ->
                            Html.div []
                                [ Html.span [ HA.class "rg-inv-num" ] [ Html.text (String.fromInt (i + 1) ++ ". ") ]
                                , Html.text name
                                ]
                        )
                        (List.take 9 items)
               )
        )


statLine : String -> String -> Html msg
statLine label value =
    Html.div [ HA.class "rg-statline" ]
        [ Html.span [ HA.class "rg-statlabel" ] [ Html.text label ]
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
        , Html.div [ HA.class "rg-hpbar-track" ]
            [ Html.div
                [ HA.class "rg-hpbar-fill"
                , HA.style "width" pct
                , HA.style "background" color
                ]
                []
            ]
        ]


logView : List String -> Html msg
logView entries =
    Html.div [ HA.class "rg-log" ]
        (List.map (\e -> Html.div [ HA.class "rg-log-entry" ] [ Html.text e ]) entries)


px : Int -> String
px n =
    String.fromInt n
