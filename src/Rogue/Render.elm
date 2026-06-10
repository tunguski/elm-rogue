module Rogue.Render exposing
    ( Scene
    , Glyph
    , Hud
    , Renderer
    , Theme
    , Popup
    , Visibility(..)
    , emptyHud
    , themeForDepth
    , defaultTheme
    , layerTerrain, layerItem, layerActor, layerHero
    )

{-| The rendering seam — the boundary that makes the *graphics engine* itself a mod.

The game engine never talks to SVG or HTML directly. Instead it produces a **`Scene`**: a flat,
renderer-agnostic description of what is on screen this frame — the terrain, which cells are
visible/remembered, the drawable `Glyph`s (hero, monsters, items) and a `Hud`. A **`Renderer msg`**
is a record of functions that turns a `Scene` into `Html msg`. Swapping `Rogue.Render.Svg` for
`Rogue.Render.Ascii` (or any third-party renderer) changes nothing in the engine — that is exactly
the "alternative game rendering engine" extension point the project promises.

Keeping the seam as *data in, Html out* (rather than the engine calling renderer methods) means a
renderer is pure and trivially swappable, and the same `Scene` can be snapshot-tested headlessly.
-}

import Html exposing (Html)
import Rogue.Grid exposing (Pos)
import Rogue.Level exposing (Level)
import Set exposing (Set)


{-| How well a cell is currently seen — drives fog-of-war dimming in renderers. -}
type Visibility
    = Visible
    | Remembered
    | Unseen


{-| One thing to draw on the grid. `char` is the ASCII/text glyph (used by text renderers and as a
fallback label); `color` is a CSS colour; `layer` orders overlapping draws (higher = on top).
`heavy` lets a renderer emphasise a glyph (e.g. the hero). -}
type alias Glyph =
    { pos : Pos
    , char : String
    , color : String
    , layer : Int
    , heavy : Bool
    }


layerTerrain : Int
layerTerrain =
    0


layerItem : Int
layerItem =
    1


layerActor : Int
layerActor =
    2


layerHero : Int
layerHero =
    3


{-| The heads-up display data a renderer paints around the map. -}
type alias Hud =
    { title : String
    , region : String
    , level : Int
    , xp : Int
    , xpNext : Int
    , depth : Int
    , hp : Int
    , maxHp : Int
    , turn : Int
    , gold : Int
    , hunger : String
    , weapon : String
    , armour : String
    , ring : String
    , statuses : List String
    , inventory : List String
    , log : List String
    , gameOver : Bool
    , won : Bool
    , status : String
    }


emptyHud : Hud
emptyHud =
    { title = "elm-rogue"
    , region = ""
    , level = 1
    , xp = 0
    , xpNext = 10
    , depth = 1
    , hp = 0
    , maxHp = 0
    , turn = 0
    , gold = 0
    , hunger = ""
    , weapon = ""
    , armour = ""
    , ring = ""
    , statuses = []
    , inventory = []
    , log = []
    , gameOver = False
    , won = False
    , status = ""
    }


{-| A visual region palette. The dungeon's look changes with depth (the Shattered-Pixel "Sewers →
Prison → Caves → Halls" progression); a `Theme` is the colour set a renderer paints terrain with, so
theming is a render concern the engine just selects by depth. -}
type alias Theme =
    { name : String
    , wallLit : String
    , wallDim : String
    , floorLit : String
    , floorDim : String
    , door : String
    }


{-| The four default regions, four floors apart, mirroring the source game's act structure. -}
themeForDepth : Int -> Theme
themeForDepth depth =
    if depth <= 2 then
        { name = "Sewers", wallLit = "#2f4a46", wallDim = "#172724", floorLit = "#10211d", floorDim = "#0a1411", door = "#3c7a5a" }

    else if depth <= 4 then
        { name = "Prison", wallLit = "#4a4334", wallDim = "#241f18", floorLit = "#1c1813", floorDim = "#100d0a", door = "#8a5a30" }

    else if depth <= 6 then
        { name = "Caves", wallLit = "#523a36", wallDim = "#281c1a", floorLit = "#1f1614", floorDim = "#120c0b", door = "#a04e3c" }

    else
        { name = "Halls", wallLit = "#41355c", wallDim = "#1f1830", floorLit = "#181226", floorDim = "#0d0916", door = "#6a4ca0" }


defaultTheme : Theme
defaultTheme =
    themeForDepth 1


{-| Everything a renderer needs for one frame. `camera` is the cell the view should centre on (the
hero), letting a renderer scroll a viewport over a large map instead of drawing every cell. -}
type alias Scene =
    { level : Level
    , visible : Set ( Int, Int )
    , explored : Set ( Int, Int )
    , glyphs : List Glyph
    , popups : List Popup
    , theme : Theme
    , camera : Pos
    , cursor : Maybe Pos
    , hud : Hud
    }


{-| A transient floating label (e.g. a combat number) drawn above a cell for one frame. -}
type alias Popup =
    { pos : Pos
    , text : String
    , color : String
    }


{-| A renderer: a named record of functions that draws a `Scene`. Extra fields beyond `view` (the
cell size, a palette hook…) let renderers carry their own configuration without the engine knowing. -}
type alias Renderer msg =
    { name : String
    , cellSize : Int
    , view : Scene -> Html msg
    }
