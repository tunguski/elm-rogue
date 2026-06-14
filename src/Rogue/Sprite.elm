module Rogue.Sprite exposing
    ( Sprite
    , resolve
    , toSvg
    , tileSprite
    )

{-| A tiny pixel-art layer for the SVG renderer.

The dungeon used to draw every actor and item as a single text glyph (`@`, `r`, `!`). This module
gives the same renderer an **original, Pixel-Dungeon-flavoured pixel-art look** without the engine
knowing anything about it: a `Sprite` is a small grid of palette-indexed pixels, and `toSvg` paints
it into one map cell as a handful of `<rect>`s (horizontally run-merged so a 12×12 sprite is a dozen
nodes, not 144). The renderer asks `resolve` for the sprite belonging to a `Glyph` — keyed first by
its semantic `sprite` id (the entity's id, e.g. "rat"), then by its category `char` (so every potion
shares one `!` sprite) — and falls back to drawing the text glyph when nothing matches.

All art here is hand-authored for this project; it merely evokes the genre's chunky 16-bit dungeon
look rather than copying any real game's assets. Pixels index a single shared `palette`, so sprites
stay compact and the whole dungeon reads as one coherent set.
-}

import Rogue.Grid exposing (Pos)
import Svg exposing (Svg, rect)
import Svg.Attributes as SA
import Html.Attributes as HA


{-| A pixel grid: `rows` are top-to-bottom, each character indexes `palette` (`.`/space = transparent).
`res` is the grid's width/height in pixels (sprites are square). -}
type alias Sprite =
    { res : Int
    , rows : List String
    }


{-| The shared colour palette. One character per colour keeps the sprite rows terse and the whole set
visually consistent (everything outlines with the same near-black, etc.). `.`/space are transparent. -}
palette : Char -> String
palette c =
    case c of
        'X' -> "#0a0c11"
        'k' -> "#161a22"
        'd' -> "#3a4150"
        'a' -> "#7a8498"
        'l' -> "#c2ccdc"
        'w' -> "#f1f5fb"
        's' -> "#aeb9cc"
        'i' -> "#5d6678"
        'r' -> "#d04a3a"
        'R' -> "#8c2c22"
        'o' -> "#e08a3c"
        'y' -> "#ecd24c"
        'G' -> "#d6b24c"
        'g' -> "#5cba57"
        'E' -> "#2f7a3a"
        'b' -> "#4f8bdc"
        'B' -> "#294a86"
        'c' -> "#5dd4cf"
        'p' -> "#a85ec8"
        'v' -> "#6a4ca0"
        'm' -> "#e07ab0"
        'n' -> "#9a6b3a"
        'N' -> "#5e3f22"
        'f' -> "#e7b487"
        't' -> "#b8884a"
        'h' -> "#6e4a2a"
        'e' -> "#caa86a"
        _ -> ""


{-| Pick the sprite for a glyph: its semantic id wins (rich, per-monster art), then the category
`char` (one sprite for every potion / scroll / weapon …). `Nothing` lets the renderer draw text. -}
resolve : String -> String -> Maybe Sprite
resolve spriteId char =
    case sheet spriteId of
        Just s ->
            Just s

        Nothing ->
            sheet char


{-| Render a sprite into the cell at `pos`, scaled to `cell` pixels, optionally tinted (a colour the
whole sprite is washed toward for fog/lighting; "" = none). Rows merge into horizontal run `<rect>`s. -}
toSvg : Int -> Pos -> String -> Float -> Sprite -> List (Svg msg)
toSvg cell pos tint tintAlpha sprite =
    let
        pxf =
            toFloat cell / toFloat sprite.res

        ox =
            toFloat (pos.x * cell)

        oy =
            toFloat (pos.y * cell)

        rowSvg : Int -> String -> List (Svg msg)
        rowSvg y row =
            runs (String.toList row) 0 []
                |> List.map
                    (\run ->
                        rect
                            [ SA.x (ff (ox + toFloat run.start * pxf))
                            , SA.y (ff (oy + toFloat y * pxf))
                            , SA.width (ff (toFloat run.len * pxf + 0.5))
                            , SA.height (ff (pxf + 0.5))
                            , SA.fill run.color
                            , HA.style "shape-rendering" "crispEdges"
                            ]
                            []
                    )
    in
    List.concat (List.indexedMap rowSvg sprite.rows)
        ++ tintLayer cell pos tint tintAlpha


tintLayer : Int -> Pos -> String -> Float -> List (Svg msg)
tintLayer cell pos tint tintAlpha =
    if tint == "" || tintAlpha <= 0 then
        []

    else
        [ rect
            [ SA.x (ff (toFloat (pos.x * cell)))
            , SA.y (ff (toFloat (pos.y * cell)))
            , SA.width (ff (toFloat cell))
            , SA.height (ff (toFloat cell))
            , SA.fill tint
            , HA.style "opacity" (ff tintAlpha)
            , HA.style "pointer-events" "none"
            ]
            []
        ]


type alias Run =
    { start : Int, len : Int, color : String }


{-| Collapse a row of palette chars into maximal same-colour horizontal runs (skipping transparent). -}
runs : List Char -> Int -> List Run -> List Run
runs chars idx acc =
    case chars of
        [] ->
            List.reverse acc

        c :: rest ->
            let
                col =
                    palette c
            in
            if col == "" then
                runs rest (idx + 1) acc

            else
                let
                    same =
                        takeWhileColor col rest

                    len =
                        1 + List.length same
                in
                runs (List.drop (List.length same) rest) (idx + len) ({ start = idx, len = len, color = col } :: acc)


takeWhileColor : String -> List Char -> List Char
takeWhileColor col chars =
    case chars of
        [] ->
            []

        c :: rest ->
            if palette c == col then
                c :: takeWhileColor col rest

            else
                []


ff : Float -> String
ff =
    String.fromFloat



-- SHEET ------------------------------------------------------------------------------------------


{-| Map a key (entity id or category char) to its sprite. Returns `Nothing` for keys with no art yet
so the renderer can fall back to the text glyph. -}
sheet : String -> Maybe Sprite
sheet key =
    case key of
        -- Hero — a per-class sprite, with the bare-handed adventurer as the fallback.
        "@" -> Just hero
        "hero" -> Just hero
        "hero-warrior" -> Just heroWarrior
        "hero-mage" -> Just heroMage
        "hero-rogue" -> Just heroRogue
        "hero-huntress" -> Just heroHuntress
        "hero-duelist" -> Just heroDuelist

        -- Monsters.
        "rat" -> Just rat
        "marsupial-rat" -> Just rat
        "snake" -> Just snake
        "gnoll-scout" -> Just gnoll
        "gnoll-archer" -> Just gnoll
        "gnoll-brute" -> Just gnollBrute
        "gnoll-shaman" -> Just shaman
        "gnoll-warlord" -> Just gnollBrute
        "crab" -> Just crab
        "skeleton" -> Just skeleton
        "swarm" -> Just swarm
        "slime" -> Just slime
        "thief" -> Just thief
        "prison-guard" -> Just guard
        "cave-bat" -> Just bat
        "piranha" -> Just piranha
        "stone-golem" -> Just golem
        "necromancer" -> Just necromancer
        "dwarf-monk" -> Just monk
        "demon" -> Just demon
        "succubus" -> Just succubus
        "ghost-ally" -> Just ghost
        "spider-queen" -> Just spiderQueen
        "dwarf-king" -> Just dwarfKing
        "yog-dzewa" -> Just yogDzewa

        -- Dungeon objects.
        "chest" -> Just chest
        "statue" -> Just statue
        "well" -> Just well
        "plant" -> Just plant
        "altar" -> Just altar
        "amulet" -> Just amulet
        "key" -> Just keyItem

        -- Item categories (keyed by glyph char).
        "!" -> Just potion
        "?" -> Just scroll
        "/" -> Just weapon
        "[" -> Just armour
        "=" -> Just ring
        "-" -> Just wand
        "%" -> Just food
        "$" -> Just gold
        "*" -> Just gem

        -- Specific consumables/objects (keyed by item id, which beats the category char).
        "bomb" -> Just bomb
        "ankh" -> Just ankh
        "torch" -> Just torch

        _ ->
            Nothing


{-| A pixel-art floor/wall/etc. tile, tinted to the region's two theme colours (`lit`,`dim`). Tiles
are separate from `sheet` because they take runtime colours rather than the fixed palette. Returns a
12×12 sprite using two passed colours plus a darker mortar line, or `Nothing` for a flat fill. -}
tileSprite : String -> Maybe Sprite
tileSprite _ =
    Nothing



-- ART --------------------------------------------------------------------------------------------
-- 12×12 grids. '.' transparent. See `palette` for the colour letters.


hero : Sprite
hero =
    Sprite 12
        [ "............"
        , "...XXXX....."
        , "..XaaaaX...."
        , "..XfwfwX...."
        , "..XffffX...."
        , "...XffX....."
        , "..XEEEEX..Xs"
        , ".XEEEEEEXXs."
        , ".XbEEbEX.s.."
        , ".XEEEEEX...."
        , "..Xt..tX...."
        , "..XX..XX...."
        ]


heroWarrior : Sprite
heroWarrior =
    Sprite 12
        [ "....XrrX...."
        , "...XaaaaX..."
        , "..XaffffaX.."
        , "..XafwwfaX.."
        , "...XffffX..."
        , "...XiiiiX..."
        , "..XiiiiiiX.s"
        , ".XiisiisiXs."
        , ".XiiiiiiiXs."
        , "..XiiiiiX..."
        , "..Xd..dX...."
        , "..XX..XX...."
        ]


heroMage : Sprite
heroMage =
    Sprite 12
        [ "....XX......"
        , "...XvvX....c"
        , "..XvvvvX..c."
        , "..XffffX.cw."
        , "..XfwwfX.c.."
        , "...XffX.Xn.."
        , "..XvvvvXXn.."
        , ".XvvvvvvXn.."
        , ".XvbvvbvXn.."
        , ".XvvvvvvX..."
        , "..Xv..vX...."
        , "..XX..XX...."
        ]


heroRogue : Sprite
heroRogue =
    Sprite 12
        [ "...XXXX....."
        , "..XkkkkX...."
        , "..XkffkX...."
        , "..XfwwfX...."
        , "..XkffkX...."
        , "...XvvX....."
        , "..XvvvvX.s.."
        , ".XvvvvvvXs.."
        , ".XvvvvvvX..."
        , ".XvvvvvvX..."
        , "..Xv..vX...."
        , "..XX..XX...."
        ]


heroHuntress : Sprite
heroHuntress =
    Sprite 12
        [ "....XX...n.."
        , "...XhhX.XnX."
        , "..XhffhXnX.."
        , "..XfwwfXn..."
        , "...XffXXn..."
        , "...XffX.n..."
        , "..XEEEEXn..."
        , ".XEEEEEEn..."
        , ".XEgEEgEn..."
        , ".XEEEEEX..."
        , "..Xt..tX...."
        , "..XX..XX...."
        ]


heroDuelist : Sprite
heroDuelist =
    Sprite 12
        [ "............"
        , "...XhhhX...."
        , "..XhffhhX..."
        , "..XfwwffX..."
        , "...XffX....."
        , "..XttttX...s"
        , ".XttttttXXs."
        , ".XteeetXs..."
        , ".XttttttX..."
        , "..Xtt.tX...."
        , "..Xe..eX...."
        , "..XX..XX...."
        ]


rat : Sprite
rat =
    Sprite 12
        [ "............"
        , "............"
        , "..X......X.."
        , ".XnX....XnX."
        , ".XnnXXXXnnX."
        , "XnnnnnnnnnnX"
        , "XnXwnnnwXnnX"
        , "XnnnnnnnnnnX"
        , ".XnnnnnnnnX."
        , "..XXnXXnXX.."
        , "....X..X...."
        , "....m..m...."
        ]


snake : Sprite
snake =
    Sprite 12
        [ "............"
        , "......XXX..."
        , ".....XEgEX.."
        , ".....XwEwX.."
        , "......XEEX.."
        , "....XXEEX..."
        , "...XEgEX...."
        , "..XEgEX....."
        , "..XEEX......"
        , "...XEgEX...."
        , "....XEgEX..."
        , ".....XXX...."
        ]


gnoll : Sprite
gnoll =
    Sprite 12
        [ "............"
        , "...X....X..."
        , "...XnX.XnX.."
        , "..XnnnXnnnX."
        , "..XnrXXrnX.."
        , "..XnnnnnnX.."
        , "...XnwwnX..."
        , "..XEEEEEEX.."
        , ".XEEnEEnEEX."
        , "..XEEEEEEX.."
        , "..Xt.XX.tX.."
        , "..XX....XX.."
        ]


gnollBrute : Sprite
gnollBrute =
    Sprite 12
        [ "...X....X..."
        , "..XnX..XnX.."
        , "..XnnXXnnX.."
        , ".XnnrXXrnnX."
        , ".XnnnnnnnnX."
        , ".XXnwwwwnXX."
        , "XRRREEEERRRX"
        , "XRREEEEEERRX"
        , ".XEEnnnnEEX."
        , ".XEEEEEEEEX."
        , ".Xtt.XX.ttX."
        , ".XX......XX."
        ]


shaman : Sprite
shaman =
    Sprite 12
        [ "............"
        , "....XXXX...."
        , "...XnnnnX..."
        , "...XnyynX..c"
        , "...XnwwnX.c."
        , "...XvvvvX.c."
        , "..XvvvvvvXc."
        , ".cXvccvvvX.."
        , ".XvvvvvvvvX."
        , "..XvvvvvvX.."
        , "...Xv..vX..."
        , "...XX..XX..."
        ]


crab : Sprite
crab =
    Sprite 12
        [ "............"
        , "............"
        , ".r........r."
        , "Xr.X.XX.X.rX"
        , ".XXrXXXXrXX."
        , "XrXroooorXrX"
        , "XrXowwwwoXrX"
        , ".XXooooooXX."
        , "..XoXXXXoX.."
        , "..XX....XX.."
        , "..X......X.."
        , ".XX......XX."
        ]


skeleton : Sprite
skeleton =
    Sprite 12
        [ "............"
        , "...XXXX....."
        , "..XllllX...."
        , "..XlXlXl...."
        , "..Xlllll...."
        , "...XllX....."
        , "..XlXlXlX..."
        , ".X.XlllX.X.."
        , "...XlXlX...."
        , "...Xl.lX...."
        , "..Xl..lX...."
        , "..XX..XX...."
        ]


swarm : Sprite
swarm =
    Sprite 12
        [ "...X........"
        , ".XkXk...Xk.."
        , "..Xk...XkXk."
        , "....XkX....."
        , ".Xk..k..Xk.."
        , "XkXk.XkX.k.."
        , "..k..XkXk..."
        , "...XkX...Xk."
        , ".Xk...Xk.kX."
        , "..XkXk..Xk.."
        , "...k..Xk...."
        , "......k....."
        ]


slime : Sprite
slime =
    Sprite 12
        [ "............"
        , "............"
        , "....XXXX...."
        , "..XEgggEX..."
        , ".XEgggggEX.."
        , ".XgwgggwgX.."
        , ".XgggggggX.."
        , ".XgXgggXgX.."
        , ".XEgggggEX.."
        , "..XEgEgEX..."
        , "...XX.XX...."
        , "............"
        ]


thief : Sprite
thief =
    Sprite 12
        [ "............"
        , "...XXXX....."
        , "..XvvvvX...."
        , "..XfyyfX...."
        , "..XffffX...."
        , "...XvvX....."
        , "..XvvvvX.G.."
        , ".XvvvvvvXG.."
        , ".XvBvvBvX..."
        , ".XvvvvvvX..."
        , "..Xv..vX...."
        , "..XX..XX...."
        ]


guard : Sprite
guard =
    Sprite 12
        [ "....XXXX...."
        , "...XaaaaX..s"
        , "...XaffaX.s."
        , "...XfwwfX.s."
        , "....XffX..s."
        , "...XiiiiX.s."
        , "..XiisiiiXs."
        , ".XiiisiiiX.."
        , ".XiiiiiiiX.."
        , "..XiiiiiX..."
        , "..Xd..dX...."
        , "..XX..XX...."
        ]


bat : Sprite
bat =
    Sprite 12
        [ "............"
        , "............"
        , ".X........X."
        , "XvX..XX..XvX"
        , "XvvXXvvXXvvX"
        , "XvvvvRRvvvvX"
        , ".XvvrwwrvvX."
        , "..XvvvvvvX.."
        , "...XvXXvX..."
        , "....XvvX...."
        , ".....XX....."
        , "............"
        ]


piranha : Sprite
piranha =
    Sprite 12
        [ "............"
        , "............"
        , "..XX........"
        , ".XbbX....X.."
        , "XbbbbXXXXbX."
        , "XbXwbbbbbbbX"
        , "XbbwbbbbbbX."
        , "XbbbrbbbbbX."
        , ".XbbbbbbbX.."
        , "..XbXXbX...."
        , "...X..X....."
        , "............"
        ]


golem : Sprite
golem =
    Sprite 12
        [ "...XXXXXX..."
        , "..XddddddX.."
        , "..XdaXXadX.."
        , "..XdaccaadX."
        , "..XddddddX.."
        , ".XddddddddX."
        , "XdaddddddaX"
        , "XddddddddddX"
        , ".XddddddddX."
        , ".Xdd.XX.ddX."
        , ".XddX..XddX."
        , ".XXX....XXX."
        ]


necromancer : Sprite
necromancer =
    Sprite 12
        [ "....XXXX...."
        , "...XkkkkX..."
        , "...XkXXkX..."
        , "...XllllX..."
        , "...XkwwkX..."
        , "..XppppppX.."
        , ".XpppppppX.G"
        , ".XppXppXpXG."
        , ".XpppppppX.."
        , ".XppppppppX."
        , "..Xp..pX...."
        , "..XX..XX...."
        ]


monk : Sprite
monk =
    Sprite 12
        [ "............"
        , "...XXXX....."
        , "..XffffX...."
        , "..XfwwfX...."
        , "..XffffX...."
        , "...XffX....."
        , "..XooooX...."
        , ".fXoooooXf.."
        , "fXXooooXXf.."
        , ".XoooooooX.."
        , "..Xo..oX...."
        , "..XX..XX...."
        ]


demon : Sprite
demon =
    Sprite 12
        [ "..X......X.."
        , ".XrX....XrX."
        , ".XrrXXXXrrX."
        , ".XrryXXyrrX."
        , ".XrrrrrrrrX."
        , "XRRrwwwwrRRX"
        , "XRRRRRRRRRRX"
        , "XRRRyRRyRRRX"
        , ".XRRRRRRRRX."
        , ".XRR.XX.RRX."
        , ".XXX....XXX."
        , "............"
        ]


succubus : Sprite
succubus =
    Sprite 12
        [ "............"
        , "...XXXX....."
        , "..XhhhhX...."
        , "..XmffmX...."
        , "..XfwwfX...."
        , "...XmmX....."
        , "X.XmmmmX.X.."
        , ".XmmmmmmXX.."
        , "..XmppmmX..."
        , "..XmmmmmX..."
        , "..Xm..mX...."
        , "..XX..XX...."
        ]


ghost : Sprite
ghost =
    Sprite 12
        [ "............"
        , "....XXXX...."
        , "...XllllX..."
        , "..XlllllllX."
        , "..XlXllXllX."
        , "..XlllllllX."
        , "..XlllllllX."
        , "..XlllllllX."
        , "..XlllllllX."
        , "..XlXllXllX."
        , "..Xl.Xl.XlX."
        , "............"
        ]



-- BOSSES -----------------------------------------------------------------------------------------


spiderQueen : Sprite
spiderQueen =
    Sprite 12
        [ "............"
        , "..X......X.."
        , ".XkX.XX.XkX."
        , "..XkXkkXkX.."
        , "X.XkkkkkkX.X"
        , ".XkrkkkrkX.."
        , ".XkkkkkkkkX."
        , "X.XkkkkkkX.X"
        , "..XkX..XkX.."
        , ".XkX.XX.XkX."
        , ".X........X."
        , "............"
        ]


dwarfKing : Sprite
dwarfKing =
    Sprite 12
        [ "...XGGGGX..."
        , "..XGyGyGX..."
        , "...XffffX..."
        , "...XfwwfX..."
        , "...XnffnX..."
        , "..XiiiiiiX.."
        , ".XiisssiiX.."
        , ".XiiGGGiiX.."
        , ".XiiiiiiiX.."
        , "..XiiiiiX..."
        , "..Xd...dX..."
        , "..XX...XX..."
        ]


yogDzewa : Sprite
yogDzewa =
    Sprite 12
        [ "..XppppppX.."
        , ".XpRppppRpX."
        , ".XpwppppwpX."
        , "XppppppppppX"
        , "XpRppwwppRpX"
        , "XppppwwppppX"
        , "XpRpppppppRX"
        , "XppppppppppX"
        , ".XppRppRppX."
        , ".XppppppppX."
        , "..XpX..XpX.."
        , "..X......X.."
        ]



-- OBJECTS ----------------------------------------------------------------------------------------


chest : Sprite
chest =
    Sprite 12
        [ "............"
        , "..XXXXXXXX.."
        , ".XnnnnnnnnX."
        , ".XnNNNNNNnX."
        , ".XnnnnnnnnX."
        , ".XXXXXXXXXX."
        , ".XnnnGGnnnX."
        , ".XnnGXXGnnX."
        , ".XnnGXXGnnX."
        , ".XnnnGGnnnX."
        , ".XnnnnnnnnX."
        , "..XXXXXXXX.."
        ]


statue : Sprite
statue =
    Sprite 12
        [ "....XXXX...."
        , "...XaaaaX..."
        , "...XaddaX..."
        , "...XaaaaX..."
        , "....XaaX...."
        , "...XaaaaX..."
        , "..XaaaaaaX.."
        , "..XaaaaaaX.."
        , "..XaaaaaaX.."
        , ".XddddddddX."
        , ".XddddddddX."
        , ".XXXXXXXXXX."
        ]


well : Sprite
well =
    Sprite 12
        [ "............"
        , "....XbbX...."
        , "...XbwbbX..."
        , "..XnnnnnnX.."
        , "..XnbbbbnX.."
        , "..XnbwbbnX.."
        , "..XnbbbbnX.."
        , "..XnnnnnnX.."
        , "...XnnnnX..."
        , "...XnnnnX..."
        , "..XnnnnnnX.."
        , "..XXXXXXXX.."
        ]


plant : Sprite
plant =
    Sprite 12
        [ "............"
        , ".....g......"
        , "....ggg....."
        , "...gg.gg...."
        , "..g.gEg.g..."
        , "....gEg....."
        , "....gEg....."
        , "....XnX....."
        , "...XnnnX...."
        , "...XeeeX...."
        , "...XeeeX...."
        , "....XXX....."
        ]


altar : Sprite
altar =
    Sprite 12
        [ "............"
        , "....XwwX...."
        , "...XbwwbX..."
        , "..XaaaaaaX.."
        , "..XaaaaaaX.."
        , ".XddddddddX."
        , ".XnnnnnnnnX."
        , ".XddddddddX."
        , ".XnnnnnnnnX."
        , ".XddddddddX."
        , ".XnnnnnnnnX."
        , ".XXXXXXXXXX."
        ]


amulet : Sprite
amulet =
    Sprite 12
        [ "...X....X..."
        , "....X..X...."
        , ".....XX....."
        , "....XGGX...."
        , "...XGGGGX..."
        , "..XGGppGGX.."
        , "..XGpwppGX.."
        , "..XGppppGX.."
        , "..XGGppGGX.."
        , "...XGGGGX..."
        , "....XGGX...."
        , ".....XX....."
        ]


keyItem : Sprite
keyItem =
    Sprite 12
        [ "............"
        , "...XGGGX...."
        , "..XGyyyGX..."
        , "..XGyXyGX..."
        , "..XGyyyGX..."
        , "...XGGGX...."
        , "....XGX....."
        , "....XGX....."
        , "....XGX....."
        , "....XGGX...."
        , "....XGXGX..."
        , "....XGGGX..."
        ]



-- ITEMS ------------------------------------------------------------------------------------------


potion : Sprite
potion =
    Sprite 12
        [ "............"
        , ".....XX....."
        , ".....Xw....."
        , ".....XX....."
        , "....XrrX...."
        , "...XrrrrX..."
        , "..XrwrrrrX.."
        , "..XrrrrrrX.."
        , "..XrwrrrrX.."
        , "..XrrrrrrX.."
        , "...XrrrrX..."
        , "....XXXX...."
        ]


scroll : Sprite
scroll =
    Sprite 12
        [ "............"
        , "..XXXXXXXX.."
        , ".XeeeeeeeeX."
        , ".XeXeeXeeeX."
        , ".XeeeeeeeeX."
        , ".XeeXeeeXeX."
        , ".XeeeeeeeeX."
        , ".XeXeeeeeeX."
        , ".XeeeeeeeeX."
        , ".XeeeeeeeeX."
        , "..XXXXXXXX.."
        , "....p..p...."
        ]


weapon : Sprite
weapon =
    Sprite 12
        [ "..........X."
        , ".........XsX"
        , "........XssX"
        , ".......XssX."
        , "......XssX.."
        , ".....XssX..."
        , "....XssX...."
        , "...XssX....."
        , ".G.XsX......"
        , "GnGXX......."
        , ".GnG........"
        , "..G........."
        ]


armour : Sprite
armour =
    Sprite 12
        [ "............"
        , "..XiXXiX...."
        , ".XiiiiiiX..."
        , "XiiiiiiiiX.."
        , "XiisssiiiX.."
        , "XiisiisiiX.."
        , "XiiiiiiiiX.."
        , ".XiiiiiiX..."
        , ".XiiiiiiX..."
        , "..XiiiiX...."
        , "..XiiiiX...."
        , "...XXXX....."
        ]


ring : Sprite
ring =
    Sprite 12
        [ "............"
        , ".....cc....."
        , "....cXXc...."
        , "...XGGGGX..."
        , "..XGGXXGGX.."
        , "..XGX..XGX.."
        , "..XGX..XGX.."
        , "..XGGXXGGX.."
        , "...XGGGGX..."
        , "....XXXX...."
        , "............"
        , "............"
        ]


wand : Sprite
wand =
    Sprite 12
        [ "..........c."
        , ".........cwc"
        , "........cwXc"
        , ".......XwXc."
        , "......XnX..."
        , ".....XnX...."
        , "....XnX....."
        , "...XnX......"
        , "..XnX......."
        , ".XnX........"
        , "XGX........."
        , "GX.........."
        ]


food : Sprite
food =
    Sprite 12
        [ "............"
        , "...XXXXXX..."
        , "..XnnnnnnX.."
        , ".XneeeeeenX."
        , ".XneeeeeenX."
        , ".XneeXeeenX."
        , ".XneeeeeenX."
        , ".XneeeXeenX."
        , ".XneeeeeenX."
        , "..XnnnnnnX.."
        , "...XXXXXX..."
        , "............"
        ]


gem : Sprite
gem =
    Sprite 12
        [ "............"
        , ".....XX....."
        , "....XppX...."
        , "...XppppX..."
        , "..XpwppppX.."
        , "..XppppppX.."
        , "..XppppppX.."
        , "...XppppX..."
        , "....XppX...."
        , ".....XX....."
        , "............"
        , "............"
        ]


bomb : Sprite
bomb =
    Sprite 12
        [ ".......XX..."
        , "......Xy...."
        , ".....Xy....."
        , "....XXX....."
        , "..XkkkkX...."
        , ".XkkkkkkX..."
        , "XkkwkkkkkX.."
        , "XkkkkkkkkX.."
        , "XkkkkkkkkX.."
        , ".XkkkkkkX..."
        , "..XkkkkX...."
        , "....XX......"
        ]


ankh : Sprite
ankh =
    Sprite 12
        [ "...XccX....."
        , "..Xc..cX...."
        , "..Xc..cX...."
        , "...XccX....."
        , ".XcccccccX.."
        , "...XccX....."
        , "...XccX....."
        , "...XccX....."
        , "...XccX....."
        , "...XccX....."
        , "............"
        , "............"
        ]


torch : Sprite
torch =
    Sprite 12
        [ ".....o......"
        , "....ooo....."
        , "....oyo....."
        , "...oyyyo...."
        , "....ooo....."
        , ".....X......"
        , ".....n......"
        , ".....n......"
        , ".....n......"
        , ".....n......"
        , ".....n......"
        , "............"
        ]


gold : Sprite
gold =
    Sprite 12
        [ "............"
        , "............"
        , "...XX...XX.."
        , "..XGGX.XGGX."
        , "..XGyGXGyGX."
        , "..XGGGGGGGX."
        , ".XGGyGGGyGX."
        , ".XGGGGGGGGX."
        , ".XGyGGGyGGX."
        , ".XGGGGGGGGX."
        , "..XGGGGGGX.."
        , "...XXXXXX..."
        ]
