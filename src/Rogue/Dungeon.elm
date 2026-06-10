module Rogue.Dungeon exposing
    ( GenConfig
    , defaultConfig
    , Generated
    , Room
    , generate
    , roomCenter
    )

{-| A seeded dungeon generator in the classic "rooms + corridors" style (the rogue/NetHack lineage,
which Shattered Pixel Dungeon's standard floors also descend from).

It carves up to `maxRooms` non-overlapping rectangular rooms out of solid rock, links each new room
to the previous one with an L-shaped corridor, drops a door where a corridor pierces a room wall,
and finally places the up- and down-stairs in the first and last rooms. Everything is threaded
through `Rogue.Rng.Seed`, so a config + seed always yields the same floor — essential for
reproducible play and headless tests.

The generator is intentionally content-free: it returns the `Level` plus the room rectangles and the
two stair positions, and leaves enemy/item population to the engine (which consults the moddable
`Ruleset`). That keeps generation reusable across mods that only change *what* fills a floor.
-}

import Rogue.Grid as Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Rng as Rng exposing (Seed)
import Rogue.Tile exposing (Tile(..))


{-| Tunable generation parameters. A mod can supply its own `GenConfig` for bigger/denser floors. -}
type alias GenConfig =
    { width : Int
    , height : Int
    , maxRooms : Int
    , minRoomSize : Int
    , maxRoomSize : Int
    }


defaultConfig : GenConfig
defaultConfig =
    { width = 40
    , height = 26
    , maxRooms = 12
    , minRoomSize = 4
    , maxRoomSize = 9
    }


{-| An axis-aligned room rectangle, in tile coordinates (inclusive of `x,y`, exclusive of the far
edge — `w`/`h` are the carved interior size). -}
type alias Room =
    { x : Int, y : Int, w : Int, h : Int }


{-| The result of generation: the tile map, the rooms in placement order, and the stairs. -}
type alias Generated =
    { level : Level
    , rooms : List Room
    , stairsUp : Pos
    , stairsDown : Pos
    , seed : Seed
    }


roomCenter : Room -> Pos
roomCenter r =
    { x = r.x + r.w // 2, y = r.y + r.h // 2 }


roomsOverlap : Room -> Room -> Bool
roomsOverlap a b =
    -- Treat rooms as expanded by 1 so they never share a wall (keeps a gap of rock between them).
    (a.x - 1)
        < (b.x + b.w + 1)
        && (a.x + a.w + 1)
        > (b.x - 1)
        && (a.y - 1)
        < (b.y + b.h + 1)
        && (a.y + a.h + 1)
        > (b.y - 1)


generate : GenConfig -> Seed -> Generated
generate cfg seed0 =
    let
        blank =
            Level.empty cfg.width cfg.height

        ( placedRooms, level1, seed1 ) =
            placeRooms cfg cfg.maxRooms [] blank seed0

        rooms =
            List.reverse placedRooms

        level2 =
            connectRooms rooms level1

        ( upPos, downPos, level3 ) =
            placeStairs rooms level2

        level4 =
            addWalls level3
    in
    { level = level4
    , rooms = rooms
    , stairsUp = upPos
    , stairsDown = downPos
    , seed = seed1
    }


{-| Try `attempts` times to drop a room that doesn't collide with an existing one; carve the ones
that fit. Accumulates rooms in reverse placement order. -}
placeRooms : GenConfig -> Int -> List Room -> Level -> Seed -> ( List Room, Level, Seed )
placeRooms cfg attempts acc level seed =
    if attempts <= 0 then
        ( acc, level, seed )

    else
        let
            ( rw, s1 ) =
                Rng.range cfg.minRoomSize cfg.maxRoomSize seed

            ( rh, s2 ) =
                Rng.range cfg.minRoomSize cfg.maxRoomSize s1

            ( rx, s3 ) =
                Rng.range 1 (cfg.width - rw - 2) s2

            ( ry, s4 ) =
                Rng.range 1 (cfg.height - rh - 2) s3

            room =
                { x = rx, y = ry, w = rw, h = rh }
        in
        if List.any (roomsOverlap room) acc then
            placeRooms cfg (attempts - 1) acc level s4

        else
            placeRooms cfg (attempts - 1) (room :: acc) (carveRoom room level) s4


carveRoom : Room -> Level -> Level
carveRoom room level =
    List.foldl
        (\p lv -> Level.set p Floor lv)
        level
        (roomCells room)


roomCells : Room -> List Pos
roomCells room =
    List.concatMap
        (\y -> List.map (\x -> { x = x, y = y }) (List.range room.x (room.x + room.w - 1)))
        (List.range room.y (room.y + room.h - 1))


{-| Link each room to the previous one with an L-shaped corridor. -}
connectRooms : List Room -> Level -> Level
connectRooms rooms level =
    case rooms of
        [] ->
            level

        first :: rest ->
            Tuple.second (List.foldl connectStep ( first, level ) rest)


connectStep : Room -> ( Room, Level ) -> ( Room, Level )
connectStep room ( prev, level ) =
    ( room, carveCorridor (roomCenter prev) (roomCenter room) level )


{-| Carve an L-shaped corridor between two points (horizontal leg then vertical leg). Cells that were
solid wall become `Floor`; where the corridor first breaches a carved room we leave a `Door`. -}
carveCorridor : Pos -> Pos -> Level -> Level
carveCorridor a b level =
    let
        horizontal =
            List.map (\x -> { x = x, y = a.y }) (rangeBetween a.x b.x)

        vertical =
            List.map (\y -> { x = b.x, y = y }) (rangeBetween a.y b.y)
    in
    List.foldl carveCorridorCell level (horizontal ++ vertical)


carveCorridorCell : Pos -> Level -> Level
carveCorridorCell p level =
    case Level.at p level of
        Empty ->
            Level.set p Floor level

        Wall ->
            Level.set p Floor level

        _ ->
            -- Already floor/door/stairs — leave it.
            level


rangeBetween : Int -> Int -> List Int
rangeBetween a b =
    if a <= b then
        List.range a b

    else
        List.reverse (List.range b a)


{-| Turn every still-`Empty` cell that touches a carved cell (8-directionally) into a `Wall`, so
rooms and corridors get a solid one-tile rock border. This is what gives walls to collide with and to
block line of sight; the far void stays `Empty`. -}
addWalls : Level -> Level
addWalls level =
    List.foldl
        (\p lv ->
            if Level.at p lv == Empty && touchesCarved p lv then
                Level.set p Wall lv

            else
                lv
        )
        level
        (Level.positions level)


touchesCarved : Pos -> Level -> Bool
touchesCarved p level =
    List.any
        (\nb ->
            case Level.at nb level of
                Empty ->
                    False

                Wall ->
                    False

                _ ->
                    True
        )
        (Grid.neighbors8 p)


{-| Up-stairs in the first room's centre, down-stairs in the last room's centre. Falls back to a
default position if there are somehow no rooms. -}
placeStairs : List Room -> Level -> ( Pos, Pos, Level )
placeStairs rooms level =
    case ( List.head rooms, List.head (List.reverse rooms) ) of
        ( Just firstRoom, Just lastRoom ) ->
            let
                up =
                    roomCenter firstRoom

                down =
                    roomCenter lastRoom
            in
            ( up, down, level |> Level.set up StairsUp |> Level.set down StairsDown )

        _ ->
            let
                fallback =
                    { x = 1, y = 1 }
            in
            ( fallback, fallback, level )
