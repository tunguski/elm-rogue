module Rogue.Render.Webgl exposing (renderer)

{-| A third rendering engine for the same `Scene`: a real-time **WebGL** view that lifts the dungeon
into an isometric 3-D diorama. Floors are flat tiles, walls are extruded blocks, and the hero and
monsters are little cubes that hover and bob. A single directional light (plus ambient) shades every
face, and a continuous time uniform — fed from the app's animation-frame clock — drives the idle
animations and a slowly circling light.

Like `Rogue.Render.Svg` and `Rogue.Render.Ascii`, this is a plain `Renderer` value: the engine knows
nothing about WebGL. It consumes the same renderer-agnostic `Scene`, so the toolbar can swap to "3D"
and the game plays identically. All geometry is generated here from the `Scene`; nothing in the engine
changed except a `time` field threaded onto the scene so the shaders can animate.
-}

import Char
import Html exposing (Html)
import Html.Attributes as HA
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Rogue.Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render as Render exposing (Glyph, Hud, Scene)
import Rogue.Tile as Tile exposing (Tile(..))
import Set
import WebGL exposing (Mesh, Shader)


renderer : Render.Renderer msg
renderer =
    { name = "3D"
    , cellSize = 24
    , view = view
    }


canvasW : Int
canvasW =
    760


canvasH : Int
canvasH =
    540



-- VIEW -------------------------------------------------------------------------------------------


view : Scene -> Html msg
view scene =
    let
        win =
            viewport scene.camera scene.level.width scene.level.height

        t =
            scene.time

        pv =
            cameraMatrix scene t

        light =
            -- A directional light that drifts in a slow circle so faces re-shade over time.
            vec3 (0.5 + 0.4 * sin (t / 2600)) 1.0 (0.4 + 0.4 * cos (t / 2600))

        terrain =
            WebGL.entity vertexShader fragmentShader (terrainMesh scene win) (uniforms pv M4.identity light (vec3 1 1 1))

        -- How far through the current tile-to-tile slide we are (eased 0..1).
        slide =
            smoothstep (clamp 0 1 ((t - scene.stepStart) / stepDuration))

        actors =
            scene.glyphs
                |> List.filter (\g -> inWindow win g.pos && Set.member ( g.pos.x, g.pos.y ) scene.visible)
                |> List.map (\g -> actorEntity pv light t (actorWorld scene slide g) g)
    in
    Html.div [ HA.class "rg-gamewrap" ]
        [ Html.div [ HA.class "rg-mapwrap" ]
            [ WebGL.toHtmlWith
                [ WebGL.depth 1
                , WebGL.antialias
                , WebGL.clearColor 0.02 0.03 0.05 1
                ]
                [ HA.width canvasW
                , HA.height canvasH
                , HA.style "width" "100%"
                , HA.style "height" "auto"
                , HA.style "max-width" (String.fromInt canvasW ++ "px")
                , HA.style "display" "block"
                , HA.style "border" "1px solid #1b2433"
                , HA.style "border-radius" "8px"
                , HA.style "background" "#05070b"
                ]
                (terrain :: actors)
            , overlayView scene.hud
            ]
        , hudView scene.hud
        ]


{-| The inclusive cell window to render, centred on the camera and clamped to the map. A touch wider
than the SVG view because the isometric camera reveals more ground. -}
viewport : Pos -> Int -> Int -> { x0 : Int, y0 : Int, x1 : Int, y1 : Int }
viewport camera width height =
    let
        vw =
            min width 19

        vh =
            min height 19

        clampStart c span extent =
            max 0 (min (extent - span) (c - span // 2))

        x0 =
            clampStart camera.x vw width

        y0 =
            clampStart camera.y vh height
    in
    { x0 = x0, y0 = y0, x1 = x0 + vw - 1, y1 = y0 + vh - 1 }


inWindow : { x0 : Int, y0 : Int, x1 : Int, y1 : Int } -> Pos -> Bool
inWindow win p =
    p.x >= win.x0 && p.x <= win.x1 && p.y >= win.y0 && p.y <= win.y1


{-| An isometric camera: an orthographic projection viewed from a fixed diagonal, looking at the hero.
A gentle bob on the eye height makes the whole diorama breathe. -}
cameraMatrix : Scene -> Float -> Mat4
cameraMatrix scene t =
    let
        cx =
            toFloat scene.camera.x + 0.5

        cz =
            toFloat scene.camera.y + 0.5

        center =
            vec3 cx 0 cz

        d =
            10.5

        eye =
            vec3 (cx + d) (d * 1.15 + 0.3 * sin (t / 1400)) (cz + d)

        viewM =
            M4.makeLookAt eye center (vec3 0 1 0)

        aspect =
            toFloat canvasW / toFloat canvasH

        hh =
            10.5

        hw =
            hh * aspect

        proj =
            M4.makeOrtho -hw hw -hh hh -60 60
    in
    M4.mul proj viewM


uniforms : Mat4 -> Mat4 -> Vec3 -> Vec3 -> Uniforms
uniforms pv model light tint =
    { pv = pv, model = model, light = light, tint = tint }


{-| Milliseconds a one-tile slide takes. -}
stepDuration : Float
stepDuration =
    150


{-| Smoothstep easing for the slide so it accelerates out and decelerates in. -}
smoothstep : Float -> Float
smoothstep p =
    p * p * (3 - 2 * p)



-- ACTORS -----------------------------------------------------------------------------------------


{-| The actor's animated world (x, z): linearly interpolated from the cell it left toward its current
cell by the eased slide progress. Actors that didn't move (or teleported) sit at their cell. -}
actorWorld : Scene -> Float -> Glyph -> ( Float, Float )
actorWorld scene slide glyph =
    let
        to =
            glyph.pos

        from =
            scene.moves
                |> List.filter (\m -> m.to == to)
                |> List.head
                |> Maybe.map .from
                |> Maybe.withDefault to

        lerp a b =
            toFloat a + (toFloat b - toFloat a) * slide
    in
    ( lerp from.x to.x + 0.5, lerp from.y to.y + 0.5 )


{-| One hovering cube per actor, tinted to its glyph colour. The hero rides higher and a little larger;
everyone bobs on a per-cell phase so a room doesn't pulse in unison. -}
actorEntity : Mat4 -> Vec3 -> Float -> ( Float, Float ) -> Glyph -> WebGL.Entity
actorEntity pv light t ( wx, wz ) glyph =
    let
        isHero =
            glyph.layer == Render.layerHero

        size =
            if isHero then
                0.6

            else if glyph.layer == Render.layerActor then
                0.46

            else
                0.34

        phase =
            toFloat (glyph.pos.x * 3 + glyph.pos.y * 5)

        bob =
            0.1 * sin (t / 320 + phase)

        baseY =
            (if isHero then
                0.62

             else
                0.5
            )
                + bob

        model =
            M4.mul
                (M4.makeTranslate (vec3 wx baseY wz))
                (M4.makeScale (vec3 size size size))
    in
    WebGL.entity vertexShader fragmentShader cubeMesh (uniforms pv model light (hexToVec3 glyph.color))



-- TERRAIN MESH -----------------------------------------------------------------------------------


terrainMesh : Scene -> { x0 : Int, y0 : Int, x1 : Int, y1 : Int } -> Mesh Vertex
terrainMesh scene win =
    let
        cells =
            List.concatMap
                (\gy -> List.map (\gx -> { x = gx, y = gy }) (List.range win.x0 win.x1))
                (List.range win.y0 win.y1)

        -- The visible torch-bearing walls: every ~10th wall carries a sconce. Precomputed so each
        -- cell can sum the warm, flickering glow of nearby torches into its colour.
        torches =
            cells
                |> List.filter (\p -> isWall (Level.at p scene.level) && isTorchWall p && Set.member ( p.x, p.y ) scene.visible)
    in
    WebGL.triangles (List.concatMap (cellGeometry scene torches) cells)


isWall : Tile -> Bool
isWall tile =
    tile == Wall || tile == SecretDoor


{-| Roughly one wall in ten bears a torch (a stable per-cell choice). -}
isTorchWall : Pos -> Bool
isTorchWall p =
    modBy 10 (p.x * 37 + p.y * 71 + 5) == 0


cellGeometry : Scene -> List Pos -> Pos -> List ( Vertex, Vertex, Vertex )
cellGeometry scene torches p =
    let
        key =
            ( p.x, p.y )

        visible =
            Set.member key scene.visible

        seen =
            visible || Set.member key scene.explored
    in
    if not seen then
        []

    else
        let
            dim =
                if visible then
                    1.0

                else
                    0.45

            tile =
                Level.at p scene.level

            prof =
                regionProfile scene.theme.name

            -- Distance fog: cells far from the hero fade toward the region's fog colour, so the iso
            -- diorama melts into depth at its edges (and each region tints that fade differently).
            fog =
                clamp 0 0.72 ((distTo scene.camera p - 5) / 13)

            -- Warm, flickering light pooled from nearby torches (only on cells in view).
            warm =
                if visible then
                    torchGlow scene.time torches p

                else
                    vec3 0 0 0

            col =
                addWarm (fogMix (V3.scale dim (tileColor scene.theme tile)) prof.fog fog) warm

            fx =
                toFloat p.x

            fz =
                toFloat p.y

            -- Per-region wall height, jittered per cell by the region's roughness (caves jagged,
            -- halls smooth) so skylines vary instead of being a flat ridge.
            wallH =
                prof.wallH + prof.rough * (hash01 p - 0.5) * 2

            torchHere =
                visible && isWall tile && isTorchWall p
        in
        case tile of
            Wall ->
                wallBlock fx fz wallH col (hash01 p)
                    ++ torchOn torchHere scene.time p fx fz wallH

            SecretDoor ->
                wallBlock fx fz wallH col (hash01 p)

            Chasm ->
                -- A sunken dark pit.
                floorQuad fx fz -0.6 (V3.scale 0.5 col)

            Water ->
                -- A gentle surface: the sheet bobs a hair and brightens/darkens in a slow swell.
                let
                    ripple =
                        sin (scene.time / 700 + (fx + fz) * 0.9)

                    y =
                        -0.12 + 0.025 * ripple

                    shimmer =
                        V3.scale (1.0 + 0.12 * ripple) col
                in
                floorQuad fx fz y shimmer

            _ ->
                floorQuad fx fz 0.0 col


{-| Warm light a cell receives from nearby torches: each contributes a flickering, distance-falloff
glow in a fire colour. Summed across torches in range and clamped by `addWarm` at the call site. -}
torchGlow : Float -> List Pos -> Pos -> Vec3
torchGlow time torches p =
    let
        warmColor =
            vec3 1.0 0.62 0.28

        contribute tc acc =
            let
                d =
                    distTo tc p
            in
            if d > 4.5 then
                acc

            else
                let
                    falloff =
                        let
                            f =
                                1 - d / 4.5
                        in
                        f * f

                    phase =
                        toFloat (tc.x * 13 + tc.y * 29)

                    flicker =
                        0.72 + 0.18 * sin (time / 110 + phase) + 0.1 * sin (time / 47 + phase * 1.7)
                in
                V3.add acc (V3.scale (0.55 * falloff * flicker) warmColor)
    in
    List.foldl contribute (vec3 0 0 0) torches


{-| Add a warm light contribution to a colour, clamping each channel to 1 so highlights don't blow out. -}
addWarm : Vec3 -> Vec3 -> Vec3
addWarm base warm =
    vec3
        (min 1 (V3.getX base + V3.getX warm))
        (min 1 (V3.getY base + V3.getY warm))
        (min 1 (V3.getZ base + V3.getZ warm))


{-| The torch itself: a small flickering flame quad on the wall's camera-facing (south) side near the
top, bright warm so it reads as the light's source. Only drawn when `on`. -}
torchOn : Bool -> Float -> Pos -> Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
torchOn on time p fx fz h =
    if not on then
        []

    else
        let
            phase =
                toFloat (p.x * 13 + p.y * 29)

            flick =
                0.8 + 0.2 * sin (time / 90 + phase)

            flame =
                V3.scale flick (vec3 1.0 0.7 0.3)

            cx =
                fx + 0.5

            y0 =
                h * 0.55

            y1 =
                h * 0.85

            z =
                fz + 1.02
        in
        -- a small flame patch standing slightly proud of the south face
        quad flame (vec3 0 0 1) (vec3 (cx - 0.12) y0 z) (vec3 (cx + 0.12) y0 z) (vec3 (cx + 0.12) y1 z) (vec3 (cx - 0.12) y1 z)


{-| A region's 3-D character: base wall height, how rough/jagged the wall tops are, and the colour the
distance fog fades toward. Walls climb and fog darkens as you descend toward the Demon Halls. -}
regionProfile : String -> { wallH : Float, rough : Float, fog : Vec3 }
regionProfile name =
    case name of
        "Sewers" ->
            { wallH = 0.8, rough = 0.1, fog = vec3 0.05 0.1 0.09 }

        "Prison" ->
            { wallH = 0.95, rough = 0.12, fog = vec3 0.1 0.08 0.04 }

        "Caves" ->
            { wallH = 1.1, rough = 0.35, fog = vec3 0.1 0.05 0.04 }

        "Halls" ->
            { wallH = 1.25, rough = 0.08, fog = vec3 0.06 0.04 0.12 }

        "Metropolis" ->
            { wallH = 1.35, rough = 0.15, fog = vec3 0.07 0.07 0.09 }

        _ ->
            { wallH = 1.5, rough = 0.3, fog = vec3 0.12 0.03 0.03 }


{-| Mix a colour toward the fog colour by `f` (0 = none, 1 = full fog). -}
fogMix : Vec3 -> Vec3 -> Float -> Vec3
fogMix col fog f =
    V3.add (V3.scale (1 - f) col) (V3.scale f fog)


distTo : Pos -> Pos -> Float
distTo a b =
    let
        dx =
            toFloat (a.x - b.x)

        dy =
            toFloat (a.y - b.y)
    in
    sqrt (dx * dx + dy * dy)


{-| A stable 0..1 value per cell for organic height jitter. -}
hash01 : Pos -> Float
hash01 p =
    toFloat (modBy 100 (p.x * 73 + p.y * 131 + 17)) / 100


{-| A flat floor tile (top face only) at height `y`. -}
floorQuad : Float -> Float -> Float -> Vec3 -> List ( Vertex, Vertex, Vertex )
floorQuad x z y col =
    quad col
        (vec3 0 1 0)
        (vec3 x y z)
        (vec3 (x + 1) y z)
        (vec3 (x + 1) y (z + 1))
        (vec3 x y (z + 1))


{-| An extruded wall: the top plus only the two sides the fixed isometric camera can actually see (it
looks from +x/+z, so the north and west faces are always hidden — culling them halves wall geometry
and keeps the per-frame mesh light).

A little stone detail, kept deliberately cheap: each block is tinted slightly by a per-cell hash so
neighbours differ in tone, and each visible side splits into a lighter upper course over a darker
lower course (one extra quad per face) — a subtle masonry band rather than a flat slab. -}
wallBlock : Float -> Float -> Float -> Vec3 -> Float -> List ( Vertex, Vertex, Vertex )
wallBlock x z h col tone =
    let
        x1 =
            x + 1

        z1 =
            z + 1

        -- ±8% tonal variation between blocks.
        v =
            V3.scale (0.92 + 0.16 * tone) col

        top =
            V3.scale 1.18 v

        upper =
            V3.scale 1.0 v

        lower =
            V3.scale 0.82 v

        -- the height the upper/lower courses meet at
        band =
            h * 0.62

        -- a vertical face split into a darker lower course and lighter upper course
        face n xa za xb zb =
            quad lower n (vec3 xa 0 za) (vec3 xb 0 zb) (vec3 xb band zb) (vec3 xa band za)
                ++ quad upper n (vec3 xa band za) (vec3 xb band zb) (vec3 xb h zb) (vec3 xa h za)
    in
    -- top
    quad top (vec3 0 1 0) (vec3 x h z) (vec3 x1 h z) (vec3 x1 h z1) (vec3 x h z1)
        -- south (+z) and east (+x) faces (the two the camera sees), each banded
        ++ face (vec3 0 0 1) x z1 x1 z1
        ++ face (vec3 1 0 0) x1 z1 x1 z


{-| Two triangles for a quad with a single colour and normal. Winding is unspecified because face
culling is off; the normal carries the lighting. -}
quad : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
quad col n a b c d =
    let
        v pos =
            Vertex pos col n
    in
    [ ( v a, v b, v c ), ( v a, v c, v d ) ]



-- COLOURS ----------------------------------------------------------------------------------------


tileColor : Render.Theme -> Tile -> Vec3
tileColor theme tile =
    case tile of
        Wall ->
            hexToVec3 theme.wallLit

        SecretDoor ->
            hexToVec3 theme.wallLit

        Door ->
            hexToVec3 theme.door

        LockedDoor ->
            hexToVec3 "#c9a23a"

        StairsDown ->
            hexToVec3 "#d8b24c"

        StairsUp ->
            hexToVec3 "#4f8bff"

        Water ->
            hexToVec3 "#274b6b"

        Grass ->
            hexToVec3 "#2f5a32"

        Chasm ->
            hexToVec3 "#140d20"

        _ ->
            hexToVec3 theme.floorLit


hexToVec3 : String -> Vec3
hexToVec3 raw =
    let
        s =
            String.replace "#" "" raw

        comp from =
            toFloat (hexPair (String.slice from (from + 2) s)) / 255
    in
    vec3 (comp 0) (comp 2) (comp 4)


hexPair : String -> Int
hexPair s =
    case String.toList s of
        a :: b :: _ ->
            16 * hexDigit a + hexDigit b

        _ ->
            128


hexDigit : Char -> Int
hexDigit c =
    let
        n =
            Char.toCode c
    in
    if n >= 48 && n <= 57 then
        n - 48

    else if n >= 97 && n <= 102 then
        n - 87

    else if n >= 65 && n <= 70 then
        n - 55

    else
        0



-- CUBE (shared actor mesh) -----------------------------------------------------------------------


cubeMesh : Mesh Vertex
cubeMesh =
    let
        h =
            0.5

        white =
            vec3 1 1 1

        face n a b c d =
            quad white n a b c d
    in
    WebGL.triangles <|
        List.concat
            [ face (vec3 0 1 0) (vec3 -h h -h) (vec3 h h -h) (vec3 h h h) (vec3 -h h h)
            , face (vec3 0 -1 0) (vec3 -h -h -h) (vec3 h -h -h) (vec3 h -h h) (vec3 -h -h h)
            , face (vec3 0 0 -1) (vec3 -h -h -h) (vec3 h -h -h) (vec3 h h -h) (vec3 -h h -h)
            , face (vec3 0 0 1) (vec3 -h -h h) (vec3 h -h h) (vec3 h h h) (vec3 -h h h)
            , face (vec3 -1 0 0) (vec3 -h -h -h) (vec3 -h -h h) (vec3 -h h h) (vec3 -h h -h)
            , face (vec3 1 0 0) (vec3 h -h -h) (vec3 h -h h) (vec3 h h h) (vec3 h h -h)
            ]



-- SHADERS ----------------------------------------------------------------------------------------


type alias Vertex =
    { position : Vec3
    , color : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { pv : Mat4
    , model : Mat4
    , light : Vec3
    , tint : Vec3
    }


type alias Varyings =
    { vcolor : Vec3 }


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;
        uniform mat4 pv;
        uniform mat4 model;
        uniform vec3 light;
        varying vec3 vcolor;
        void main () {
            gl_Position = pv * model * vec4(position, 1.0);
            float diff = max(dot(normalize(normal), normalize(light)), 0.0);
            float l = 0.4 + 0.6 * diff;
            vcolor = color * l;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 tint;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor * tint, 1.0);
        }
    |]



-- HUD --------------------------------------------------------------------------------------------


{-| A compact heads-up display beside the 3-D view (the SVG renderer's HUD lives in its own module, so
this carries a slim version: vitals and the recent log). -}
hudView : Hud -> Html msg
hudView hud =
    Html.div [ HA.class "rg-hud" ]
        [ Html.div [ HA.class "rg-hud-title" ] [ Html.text (hud.title ++ " · 3D") ]
        , statLine "Depth" (String.fromInt hud.depth ++ "  " ++ hud.region)
        , statLine "Level" (String.fromInt hud.level)
        , statLine "HP" (String.fromInt (max 0 hud.hp) ++ " / " ++ String.fromInt hud.maxHp)
        , statLine "Turn" (String.fromInt hud.turn)
        , statLine "Gold" (String.fromInt hud.gold)
        , if hud.weapon /= "" then
            statLine "Weapon" hud.weapon

          else
            Html.text ""
        , if hud.boss /= "" then
            Html.div [ HA.class "rg-boss-banner" ] [ Html.text ("⚔  " ++ hud.boss ++ "  ⚔") ]

          else
            Html.text ""
        , if hud.status /= "" then
            Html.div [ HA.class "rg-status-note" ] [ Html.text hud.status ]

          else
            Html.text ""
        , Html.div [ HA.class "rg-log" ]
            (List.map (\line -> Html.div [ HA.class "rg-log-line" ] [ Html.text line ]) hud.log)
        ]


statLine : String -> String -> Html msg
statLine label value =
    Html.div [ HA.class "rg-stat" ]
        [ Html.span [ HA.class "rg-stat-label" ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


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
            [ Html.div [ HA.class "rg-banner-title", HA.style "color" color ] [ Html.text title ]
            , Html.div [ HA.class "rg-banner-sub" ] [ Html.text ("Score " ++ String.fromInt hud.score) ]
            , Html.div [ HA.class "rg-banner-sub" ] [ Html.text "press R to play again" ]
            ]
