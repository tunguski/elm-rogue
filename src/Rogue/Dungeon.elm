module Rogue.Dungeon exposing
    ( GenConfig
    , defaultConfig
    , Generated
    , Room
    , Feature
    , FeatureKind(..)
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
import Rogue.Tile as Tile exposing (Tile(..))


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


{-| The result of generation: the tile map, the rooms in placement order, the stairs, and — when one
was carved — a locked **vault**: its `LockedDoor` cell and the interior cells the engine fills with
bonus loot (with a key guaranteed elsewhere on the floor). -}
type alias Generated =
    { level : Level
    , rooms : List Room
    , stairsUp : Pos
    , stairsDown : Pos
    , vaultDoor : Maybe Pos
    , vaultCells : List Pos
    , features : List Feature
    , seed : Seed
    }


{-| A tagged special room the engine populates: a `Treasure` room with extra loot, or a `Nest` packed
with extra monsters. -}
type FeatureKind
    = Treasure
    | Nest


type alias Feature =
    { kind : FeatureKind
    , cells : List Pos
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

        ( level5, seed2 ) =
            placeDoors level4 seed1

        ( vaultDoor, vaultCells, level6, seed3 ) =
            placeVault cfg rooms level5 seed2

        ( features, seed4 ) =
            pickFeatures rooms seed3
    in
    { level = level6
    , rooms = rooms
    , stairsUp = upPos
    , stairsDown = downPos
    , vaultDoor = vaultDoor
    , vaultCells = vaultCells
    , features = features
    , seed = seed4
    }


{-| Tag up to two "middle" rooms (not the start or the stair room) as a treasure room and a monster
nest, returning the cells the engine should fill. -}
pickFeatures : List Room -> Seed -> ( List Feature, Seed )
pickFeatures rooms seed =
    let
        -- Eligible: drop the first (start) and last (down-stairs) rooms.
        middle =
            rooms |> List.drop 1 |> dropLast

        ( shuffled, seed1 ) =
            Rng.shuffle middle seed
    in
    case shuffled of
        treasure :: nest :: _ ->
            ( [ { kind = Treasure, cells = roomCells treasure }, { kind = Nest, cells = roomCells nest } ], seed1 )

        [ treasure ] ->
            ( [ { kind = Treasure, cells = roomCells treasure } ], seed1 )

        [] ->
            ( [], seed1 )


dropLast : List a -> List a
dropLast xs =
    List.take (max 0 (List.length xs - 1)) xs


{-| Drop `Door` tiles at corridor pinch-points (a floor cell walled on one axis and open on the
other), about half the time, for atmosphere and to seed the open/close mechanic. -}
placeDoors : Level -> Seed -> ( Level, Seed )
placeDoors level seed =
    List.foldl
        (\p ( lv, s ) ->
            if Level.at p lv == Floor && isDoorway p lv then
                let
                    ( makeDoor, s2 ) =
                        Rng.chance 45 s

                    ( secret, s3 ) =
                        Rng.chance 14 s2
                in
                ( if makeDoor then
                    Level.set p
                        (if secret then
                            SecretDoor

                         else
                            Door
                        )
                        lv

                  else
                    lv
                , s3
                )

            else
                ( lv, s )
        )
        ( level, seed )
        (Level.positions level)


isDoorway : Pos -> Level -> Bool
isDoorway p level =
    let
        wall d =
            Level.at (Grid.add p d) level == Wall

        open d =
            Tile.isPassable (Level.at (Grid.add p d) level)
    in
    (wall Grid.dirN && wall Grid.dirS && open Grid.dirE && open Grid.dirW)
        || (wall Grid.dirE && wall Grid.dirW && open Grid.dirN && open Grid.dirS)


{-| Try to carve an extra dead-end **vault** room linked to a random existing room by a corridor whose
single entrance is a `LockedDoor`. Because the vault is a leaf (one connection) sealing it never blocks
the stairs. Returns the door cell, the vault's interior cells, the updated level and seed. -}
placeVault : GenConfig -> List Room -> Level -> Seed -> ( Maybe Pos, List Pos, Level, Seed )
placeVault cfg rooms level seed =
    let
        ( vw, s1 ) =
            Rng.range cfg.minRoomSize (cfg.minRoomSize + 2) seed

        ( vh, s2 ) =
            Rng.range cfg.minRoomSize (cfg.minRoomSize + 2) s1

        ( vx, s3 ) =
            Rng.range 1 (cfg.width - vw - 2) s2

        ( vy, s4 ) =
            Rng.range 1 (cfg.height - vh - 2) s3

        vault =
            { x = vx, y = vy, w = vw, h = vh }
    in
    if List.any (roomsOverlap vault) rooms then
        ( Nothing, [], level, s4 )

    else
        case nearestRoom (roomCenter vault) rooms of
            Nothing ->
                ( Nothing, [], level, s4 )

            Just target ->
                let
                    carved =
                        carveRoom vault level |> carveCorridor (roomCenter vault) (roomCenter target)

                    walled =
                        addWalls carved

                    path =
                        corridorPath (roomCenter vault) (roomCenter target)

                    doorCell =
                        List.filter (\p -> not (inRect p vault)) path |> List.head
                in
                case doorCell of
                    Just door ->
                        ( Just door, roomCells vault, Level.set door LockedDoor walled, s4 )

                    Nothing ->
                        ( Nothing, [], walled, s4 )


inRect : Pos -> Room -> Bool
inRect p r =
    p.x >= r.x && p.x < r.x + r.w && p.y >= r.y && p.y < r.y + r.h


nearestRoom : Pos -> List Room -> Maybe Room
nearestRoom from rooms =
    case rooms of
        [] ->
            Nothing

        first :: rest ->
            Just
                (List.foldl
                    (\r best ->
                        if Grid.manhattan (roomCenter r) from < Grid.manhattan (roomCenter best) from then
                            r

                        else
                            best
                    )
                    first
                    rest
                )


{-| The ordered cells of the L-shaped corridor between two points (horizontal leg then vertical). -}
corridorPath : Pos -> Pos -> List Pos
corridorPath a b =
    List.map (\x -> { x = x, y = a.y }) (rangeBetween a.x b.x)
        ++ List.map (\y -> { x = b.x, y = y }) (rangeBetween a.y b.y)


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
