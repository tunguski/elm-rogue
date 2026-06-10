module Rogue.Grid exposing
    ( Pos
    , Dir
    , dirN, dirS, dirE, dirW, dirNE, dirNW, dirSE, dirSW
    , cardinals, eightDirs
    , add, sub, move
    , chebyshev, manhattan
    , line
    , neighbors8, neighbors4
    )

{-| Grid geometry: integer board positions and the directions you can step in.

A `Pos` is a record `{ x, y }` with `x` increasing rightwards and `y` increasing downwards (screen
coordinates). A `Dir` is just a `Pos`-shaped delta, so movement is `add`. Kept dependency-free and
pure so it is shared by generation, field-of-view, movement and the AI.
-}


{-| A board cell. -}
type alias Pos =
    { x : Int, y : Int }


{-| A movement delta (also a `{ x, y }`). -}
type alias Dir =
    { x : Int, y : Int }


dirN : Dir
dirN =
    { x = 0, y = -1 }


dirS : Dir
dirS =
    { x = 0, y = 1 }


dirE : Dir
dirE =
    { x = 1, y = 0 }


dirW : Dir
dirW =
    { x = -1, y = 0 }


dirNE : Dir
dirNE =
    { x = 1, y = -1 }


dirNW : Dir
dirNW =
    { x = -1, y = -1 }


dirSE : Dir
dirSE =
    { x = 1, y = 1 }


dirSW : Dir
dirSW =
    { x = -1, y = 1 }


{-| The four orthogonal directions. -}
cardinals : List Dir
cardinals =
    [ dirN, dirE, dirS, dirW ]


{-| All eight directions (orthogonal + diagonal). -}
eightDirs : List Dir
eightDirs =
    [ dirN, dirNE, dirE, dirSE, dirS, dirSW, dirW, dirNW ]


add : Pos -> Dir -> Pos
add p d =
    { x = p.x + d.x, y = p.y + d.y }


sub : Pos -> Pos -> Dir
sub a b =
    { x = a.x - b.x, y = a.y - b.y }


{-| Alias for `add`, read as "move `p` one step in `d`". -}
move : Pos -> Dir -> Pos
move =
    add


{-| Chebyshev (king-move) distance — the number of 8-directional steps between two cells. -}
chebyshev : Pos -> Pos -> Int
chebyshev a b =
    max (abs (a.x - b.x)) (abs (a.y - b.y))


{-| Manhattan (taxicab) distance. -}
manhattan : Pos -> Pos -> Int
manhattan a b =
    abs (a.x - b.x) + abs (a.y - b.y)


{-| The cells on a straight line from `a` to `b` inclusive (Bresenham). Used for line-of-sight and
projectile paths. -}
line : Pos -> Pos -> List Pos
line a b =
    let
        dx =
            abs (b.x - a.x)

        dy =
            abs (b.y - a.y)

        sx =
            if a.x < b.x then
                1

            else
                -1

        sy =
            if a.y < b.y then
                1

            else
                -1
    in
    lineHelp a b dx dy sx sy (dx - dy) [ a ]


lineHelp : Pos -> Pos -> Int -> Int -> Int -> Int -> Int -> List Pos -> List Pos
lineHelp cur b dx dy sx sy err acc =
    if cur.x == b.x && cur.y == b.y then
        List.reverse acc

    else
        let
            e2 =
                2 * err

            ( nx, errAfterX ) =
                if e2 > -dy then
                    ( cur.x + sx, err - dy )

                else
                    ( cur.x, err )

            ( ny, errAfter ) =
                if e2 < dx then
                    ( cur.y + sy, errAfterX + dx )

                else
                    ( cur.y, errAfterX )

            next =
                { x = nx, y = ny }
        in
        -- Guard against a degenerate step (both branches skipped) so we never loop forever.
        if next == cur then
            List.reverse (b :: acc)

        else
            lineHelp next b dx dy sx sy errAfter (next :: acc)


{-| The eight neighbouring cells. -}
neighbors8 : Pos -> List Pos
neighbors8 p =
    List.map (add p) eightDirs


{-| The four orthogonal neighbouring cells. -}
neighbors4 : Pos -> List Pos
neighbors4 p =
    List.map (add p) cardinals
