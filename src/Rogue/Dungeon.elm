module Rogue.Dungeon exposing
    ( GenConfig
    , defaultConfig
    , configForDepth
    , Generated
    , Room
    , Feature
    , FeatureKind(..)
    , generate
    , roomCenter
    , roomCells
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
import Set exposing (Set)


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
    configForDepth 1


{-| Floor size and room budget scale gently with depth, so dungeons are large and densely roomed (the
old 40×26 / 12-room floors left most of the map as unwalkable rock). `maxRooms` is the number of
placement *attempts*; more attempts pack more rooms into the larger grid. -}
configForDepth : Int -> GenConfig
configForDepth depth =
    { width = min 72 (52 + depth * 2)
    , height = min 48 (34 + depth)
    , maxRooms = 26 + depth * 3
    , minRoomSize = 5
    , maxRoomSize = 10 + depth // 3
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

        -- A vault's locked door must never sever the route to the stairs; if it would, leave it open.
        level7 =
            ensureSolvable vaultDoor upPos downPos level6

        -- Scatter water pools and tall-grass patches (both passable, so connectivity is unaffected).
        ( level8, seed5 ) =
            decorateTerrain rooms level7 seed4
    in
    { level = level8
    , rooms = rooms
    , stairsUp = upPos
    , stairsDown = downPos
    , vaultDoor = vaultDoor
    , vaultCells = vaultCells
    , features = features
    , seed = seed5
    }


{-| Paint a few water/grass blobs onto room floors. Only `Floor` cells are converted (never stairs,
doors or walls), and both tiles are passable, so this never affects connectivity. -}
decorateTerrain : List Room -> Level -> Seed -> ( Level, Seed )
decorateTerrain rooms level seed =
    List.foldl decorateRoom ( level, seed ) rooms


decorateRoom : Room -> ( Level, Seed ) -> ( Level, Seed )
decorateRoom room ( level, seed ) =
    let
        ( roll, s1 ) =
            Rng.int 100 seed
    in
    if roll >= 38 || room.w < 4 || room.h < 4 then
        ( level, s1 )

    else
        let
            ( isGrass, s2 ) =
                Rng.chance 55 s1

            tile =
                if isGrass then
                    Grass

                else
                    Water

            ( cx, s3 ) =
                Rng.range (room.x + 1) (room.x + room.w - 2) s2

            ( cy, s4 ) =
                Rng.range (room.y + 1) (room.y + room.h - 2) s3

            blob =
                List.concatMap
                    (\dy -> List.map (\dx -> { x = cx + dx, y = cy + dy }) (List.range -2 2))
                    (List.range -2 2)
                    |> List.filter (\p -> Grid.chebyshev p { x = cx, y = cy } <= 2)

            painted =
                List.foldl
                    (\p lv ->
                        if Level.at p lv == Floor then
                            Level.set p tile lv

                        else
                            lv
                    )
                    level
                    blob
        in
        ( painted, s4 )


{-| If the only `LockedDoor` (a vault's) blocks the path from up- to down-stairs, downgrade it to an
ordinary `Door` so every floor is always completable. -}
ensureSolvable : Maybe Pos -> Pos -> Pos -> Level -> Level
ensureSolvable vaultDoor up down level =
    case vaultDoor of
        Nothing ->
            level

        Just door ->
            if stairsConnected level up down then
                level

            else
                Level.set door Door level


{-| BFS from `up` over walkable terrain (doors — even secret — count as passable; locked doors and
walls do not), reporting whether `down` is reached. -}
stairsConnected : Level -> Pos -> Pos -> Bool
stairsConnected level up down =
    connBfs level down [ up ] (Set.singleton ( up.x, up.y ))


connBfs : Level -> Pos -> List Pos -> Set ( Int, Int ) -> Bool
connBfs level goal frontier visited =
    if Set.member ( goal.x, goal.y ) visited then
        True

    else
        case frontier of
            [] ->
                False

            _ ->
                let
                    ( nf, nv ) =
                        List.foldl
                            (\cur acc ->
                                List.foldl
                                    (\nb ( fr, vis ) ->
                                        if connWalkable (Level.at nb level) && not (Set.member ( nb.x, nb.y ) vis) then
                                            ( nb :: fr, Set.insert ( nb.x, nb.y ) vis )

                                        else
                                            ( fr, vis )
                                    )
                                    acc
                                    (Grid.neighbors4 cur)
                            )
                            ( [], visited )
                            frontier
                in
                if List.isEmpty nf then
                    Set.member ( goal.x, goal.y ) visited

                else
                    connBfs level goal nf nv


connWalkable : Tile -> Bool
connWalkable tile =
    case tile of
        Wall ->
            False

        LockedDoor ->
            False

        Empty ->
            False

        Chasm ->
            False

        _ ->
            True


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
            let
                ( shape, s5 ) =
                    Rng.int 5 s4
            in
            placeRooms cfg (attempts - 1) (room :: acc) (carveRoomShaped shape room level) s5


{-| Carve a room as a solid rectangle by default, but sometimes round its corners, ring it with
pillars, or carve it as an ellipse — visual variety that still keeps the centre (and so the corridor
connection) clear. `shape` is a 0–4 roll. -}
carveRoomShaped : Int -> Room -> Level -> Level
carveRoomShaped shape room level =
    case shape of
        1 ->
            -- Rounded corners: drop the four corner cells.
            carveCells (List.filter (not << isCorner room) (roomCells room)) level

        2 ->
            -- Pillared hall: solid floor, then sparse interior pillars.
            carveCells (roomCells room) level |> addPillars room

        3 ->
            -- Elliptical chamber.
            carveCells (List.filter (inEllipse room) (roomCells room)) level

        _ ->
            carveCells (roomCells room) level


carveCells : List Pos -> Level -> Level
carveCells cells level =
    List.foldl (\p lv -> Level.set p Floor lv) level cells


isCorner : Room -> Pos -> Bool
isCorner room p =
    (p.x == room.x || p.x == room.x + room.w - 1)
        && (p.y == room.y || p.y == room.y + room.h - 1)


{-| Pillars on an interior even-grid, kept off the centre cross so the room never splits. -}
addPillars : Room -> Level -> Level
addPillars room level =
    if room.w < 5 || room.h < 5 then
        level

    else
        let
            cx =
                room.x + room.w // 2

            cy =
                room.y + room.h // 2
        in
        List.foldl
            (\p lv ->
                if
                    modBy 2 (p.x - room.x)
                        == 0
                        && modBy 2 (p.y - room.y)
                        == 0
                        && p.x
                        > room.x
                        && p.x
                        < room.x + room.w - 1
                        && p.y
                        > room.y
                        && p.y
                        < room.y + room.h - 1
                        && p.x
                        /= cx
                        && p.y
                        /= cy
                then
                    Level.set p Wall lv

                else
                    lv
            )
            level
            (roomCells room)


inEllipse : Room -> Pos -> Bool
inEllipse room p =
    let
        cx =
            toFloat room.x + toFloat (room.w - 1) / 2

        cy =
            toFloat room.y + toFloat (room.h - 1) / 2

        rx =
            toFloat room.w / 2

        ry =
            toFloat room.h / 2

        dx =
            (toFloat p.x - cx) / rx

        dy =
            (toFloat p.y - cy) / ry
    in
    dx * dx + dy * dy <= 1.05


carveRoom : Room -> Level -> Level
carveRoom room level =
    carveCells (roomCells room) level


roomCells : Room -> List Pos
roomCells room =
    List.concatMap
        (\y -> List.map (\x -> { x = x, y = y }) (List.range room.x (room.x + room.w - 1)))
        (List.range room.y (room.y + room.h - 1))


{-| Link each room to the previous one with an L-shaped corridor. -}
{-| Wire the rooms into a *graph*, not a chain: a minimum spanning tree (Prim's, short corridors,
guaranteed fully connected) plus each room's nearest-neighbour edge. The MST guarantees solvability;
the extra nearest-neighbour edges add loops, so floors have several routes instead of one snake. -}
connectRooms : List Room -> Level -> Level
connectRooms rooms level =
    let
        centers =
            List.map roomCenter rooms

        n =
            List.length centers
    in
    if n <= 1 then
        level

    else
        let
            edges =
                dedupeEdges (primMst centers ++ nearestEdges centers)
        in
        List.foldl
            (\( i, j ) lv -> carveCorridor (centerAt i centers) (centerAt j centers) lv)
            level
            edges


centerAt : Int -> List Pos -> Pos
centerAt i centers =
    List.head (List.drop i centers) |> Maybe.withDefault { x = 0, y = 0 }


{-| Prim's MST over room centres (Manhattan distance), as `( i, j )` index edges. O(n³) but n is tiny. -}
primMst : List Pos -> List ( Int, Int )
primMst centers =
    let
        n =
            List.length centers
    in
    primStep centers n (Set.singleton 0) []


primStep : List Pos -> Int -> Set Int -> List ( Int, Int ) -> List ( Int, Int )
primStep centers n inTree edges =
    if Set.size inTree >= n then
        edges

    else
        let
            -- The shortest edge from a tree node to a non-tree node.
            best =
                List.range 0 (n - 1)
                    |> List.filter (\i -> Set.member i inTree)
                    |> List.concatMap
                        (\i ->
                            List.range 0 (n - 1)
                                |> List.filter (\j -> not (Set.member j inTree))
                                |> List.map (\j -> ( edgeDist centers i j, ( i, j ) ))
                        )
                    |> minimumByFirst
        in
        case best of
            Just ( _, ( i, j ) ) ->
                primStep centers n (Set.insert j inTree) (( i, j ) :: edges)

            Nothing ->
                edges


{-| Each room's nearest-other-room edge (these create the loops on top of the MST). -}
nearestEdges : List Pos -> List ( Int, Int )
nearestEdges centers =
    let
        n =
            List.length centers
    in
    List.range 0 (n - 1)
        |> List.filterMap
            (\i ->
                List.range 0 (n - 1)
                    |> List.filter (\j -> j /= i)
                    |> List.map (\j -> ( edgeDist centers i j, ( i, j ) ))
                    |> minimumByFirst
                    |> Maybe.map Tuple.second
            )


edgeDist : List Pos -> Int -> Int -> Int
edgeDist centers i j =
    Grid.manhattan (centerAt i centers) (centerAt j centers)


{-| Normalise edges to `( min, max )` and drop duplicates. -}
dedupeEdges : List ( Int, Int ) -> List ( Int, Int )
dedupeEdges edges =
    edges
        |> List.map (\( a, b ) -> ( min a b, max a b ))
        |> Set.fromList
        |> Set.toList


minimumByFirst : List ( Int, a ) -> Maybe ( Int, a )
minimumByFirst xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl (\x best -> if Tuple.first x < Tuple.first best then x else best) first rest)


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
