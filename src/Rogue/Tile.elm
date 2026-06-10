module Rogue.Tile exposing
    ( Tile(..)
    , isPassable
    , blocksSight
    , isStairs
    , glyph
    , name
    )

{-| The terrain of a single dungeon cell.

Tiles are deliberately a small closed set: the *content* that mods customise (enemies, items,
weapons) is data in `Rogue.Content`, but the terrain vocabulary is fixed so generation, field of
view and movement can pattern-match on it exhaustively.
-}


type Tile
    = Wall
    | Floor
    | Door
    | OpenDoor
    | LockedDoor
    | SecretDoor
    | StairsDown
    | StairsUp
    | Chasm
    | Water
    | Grass
      -- An empty, never-generated cell (outside the level / unallocated).
    | Empty


{-| Can a walking creature enter this cell? -}
isPassable : Tile -> Bool
isPassable tile =
    case tile of
        Floor ->
            True

        Door ->
            True

        OpenDoor ->
            True

        StairsDown ->
            True

        StairsUp ->
            True

        Water ->
            True

        Grass ->
            True

        Wall ->
            False

        LockedDoor ->
            False

        SecretDoor ->
            False

        Chasm ->
            False

        Empty ->
            False


{-| Does this cell stop line of sight? (Closed doors and walls do; open floor does not.) -}
blocksSight : Tile -> Bool
blocksSight tile =
    case tile of
        Wall ->
            True

        Empty ->
            True

        Door ->
            -- Doors are treated as sight-blocking until stepped through; keeps rooms private.
            True

        LockedDoor ->
            True

        SecretDoor ->
            True

        OpenDoor ->
            False

        Floor ->
            False

        StairsDown ->
            False

        StairsUp ->
            False

        Chasm ->
            False

        Water ->
            False

        Grass ->
            -- Tall grass gives cover: it blocks line of sight until trampled to floor.
            True


isStairs : Tile -> Bool
isStairs tile =
    tile == StairsDown || tile == StairsUp


{-| The default ASCII glyph for a tile (the ASCII renderer and tests use this; the SVG renderer
draws shapes instead). -}
glyph : Tile -> String
glyph tile =
    case tile of
        Wall ->
            "#"

        Floor ->
            "."

        Door ->
            "+"

        OpenDoor ->
            "'"

        LockedDoor ->
            "+"

        SecretDoor ->
            "#"

        StairsDown ->
            ">"

        StairsUp ->
            "<"

        Chasm ->
            " "

        Water ->
            "~"

        Grass ->
            "\""

        Empty ->
            " "


name : Tile -> String
name tile =
    case tile of
        Wall ->
            "wall"

        Floor ->
            "floor"

        Door ->
            "door"

        OpenDoor ->
            "open door"

        LockedDoor ->
            "locked door"

        SecretDoor ->
            "secret door"

        StairsDown ->
            "stairs down"

        StairsUp ->
            "stairs up"

        Chasm ->
            "chasm"

        Water ->
            "water"

        Grass ->
            "tall grass"

        Empty ->
            "void"
