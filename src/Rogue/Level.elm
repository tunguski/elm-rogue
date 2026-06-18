module Rogue.Level exposing
    ( Level
    , empty
    , fromRows
    , at
    , set
    , inBounds
    , isPassableAt
    , blocksSightAt
    , positions
    , find
    , toRows
    , torches
    , isTorchWall
    )

{-| A dungeon level: a fixed-size rectangular grid of `Tile`s, stored sparsely in a `Dict` keyed by
`( x, y )`. Anything not in the dict reads back as `Empty`, so generators can paint onto a blank
canvas and the renderer/FOV never index out of bounds.
-}

import Dict exposing (Dict)
import Rogue.Grid as Grid exposing (Pos)
import Rogue.Tile as Tile exposing (Tile(..))


type alias Level =
    { width : Int
    , height : Int
    , tiles : Dict ( Int, Int ) Tile
    }


{-| A level of all `Wall` (the usual starting canvas — generators carve floors out of solid rock). -}
empty : Int -> Int -> Level
empty w h =
    { width = w, height = h, tiles = Dict.empty }


{-| Build a level from rows of characters (handy for tests and hand-authored maps). Recognised
glyphs: `#` wall, `.` floor, `+` door, `>` down, `<` up, ` ` empty. -}
fromRows : List String -> Level
fromRows rows =
    let
        h =
            List.length rows

        w =
            List.foldl (\r acc -> max acc (String.length r)) 0 rows

        addRow y row tiles =
            List.foldl
                (\( x, ch ) acc -> Dict.insert ( x, y ) (charToTile ch) acc)
                tiles
                (List.indexedMap Tuple.pair (String.toList row))

        allTiles =
            List.foldl identity Dict.empty (List.indexedMap (\y row -> addRow y row) rows)
    in
    { width = w, height = h, tiles = allTiles }


charToTile : Char -> Tile
charToTile ch =
    case ch of
        '#' ->
            Wall

        '.' ->
            Floor

        '+' ->
            Door

        '>' ->
            StairsDown

        '<' ->
            StairsUp

        _ ->
            Empty


{-| The tile at a position (`Empty` if unset or out of bounds). -}
at : Pos -> Level -> Tile
at p level =
    Dict.get ( p.x, p.y ) level.tiles |> Maybe.withDefault Empty


set : Pos -> Tile -> Level -> Level
set p tile level =
    if inBounds p level then
        { level | tiles = Dict.insert ( p.x, p.y ) tile level.tiles }

    else
        level


inBounds : Pos -> Level -> Bool
inBounds p level =
    p.x >= 0 && p.y >= 0 && p.x < level.width && p.y < level.height


isPassableAt : Pos -> Level -> Bool
isPassableAt p level =
    inBounds p level && Tile.isPassable (at p level)


blocksSightAt : Pos -> Level -> Bool
blocksSightAt p level =
    not (inBounds p level) || Tile.blocksSight (at p level)


{-| Every in-bounds position, row-major. -}
positions : Level -> List Pos
positions level =
    List.concatMap
        (\y -> List.map (\x -> { x = x, y = y }) (List.range 0 (level.width - 1)))
        (List.range 0 (level.height - 1))


{-| Wall cells that bear a lit torch sconce — a stable ~1-in-10 of the floor's walls. Shared by the
engine (firelight extends the hero's view, [[Fov]]) and every renderer (warm light pooled on nearby
cells), so torches sit in the same places however the floor is drawn. -}
torches : Level -> List Pos
torches level =
    List.filter (\p -> at p level == Wall && isTorchWall p) (positions level)


{-| Whether a wall at this position carries a torch — a deterministic per-cell choice (no state). -}
isTorchWall : Pos -> Bool
isTorchWall p =
    modBy 10 (p.x * 37 + p.y * 71 + 5) == 0


{-| The first position whose tile satisfies the predicate (row-major), if any. -}
find : (Tile -> Bool) -> Level -> Maybe Pos
find pred level =
    findHelp pred level (positions level)


findHelp : (Tile -> Bool) -> Level -> List Pos -> Maybe Pos
findHelp pred level ps =
    case ps of
        [] ->
            Nothing

        p :: rest ->
            if pred (at p level) then
                Just p

            else
                findHelp pred level rest


{-| Render the whole level to rows of glyphs (tests, the ASCII renderer, debugging). -}
toRows : Level -> List String
toRows level =
    List.map
        (\y ->
            String.fromList
                (List.map (\x -> tileChar (at { x = x, y = y } level)) (List.range 0 (level.width - 1)))
        )
        (List.range 0 (level.height - 1))


tileChar : Tile -> Char
tileChar tile =
    case String.toList (Tile.glyph tile) of
        c :: _ ->
            c

        [] ->
            ' '
