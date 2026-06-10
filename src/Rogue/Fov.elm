module Rogue.Fov exposing (compute, visibleFrom)

{-| Field of view: which cells the hero can currently see.

A symmetric raycast — for every cell within `radius` (Chebyshev) of the origin, walk the Bresenham
line from the origin to that cell and call it visible unless an *intervening* cell blocks sight. The
target cell itself counts as visible even if it is a wall, so you see the walls that bound a lit room
(exactly the lighting Shattered Pixel Dungeon shows). It is O(cells · radius) which is trivially fast
at dungeon sizes and, unlike recursive shadowcasting, is short and obviously correct.
-}

import Rogue.Grid as Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Set exposing (Set)


{-| The set of cells visible from `origin` within `radius`, as `( x, y )` keys. -}
compute : Int -> Pos -> Level -> Set ( Int, Int )
compute radius origin level =
    let
        candidates =
            List.concatMap
                (\dy ->
                    List.map (\dx -> { x = origin.x + dx, y = origin.y + dy })
                        (List.range -radius radius)
                )
                (List.range -radius radius)
    in
    List.foldl
        (\p acc ->
            if Grid.chebyshev origin p <= radius && Level.inBounds p level && visibleFrom origin p level then
                Set.insert ( p.x, p.y ) acc

            else
                acc
        )
        (Set.insert ( origin.x, origin.y ) Set.empty)
        candidates


{-| Is `target` visible from `origin` — i.e. does no cell strictly between them block sight? -}
visibleFrom : Pos -> Pos -> Level -> Bool
visibleFrom origin target level =
    let
        path =
            Grid.line origin target

        -- Drop the origin and the target; only the cells *between* can occlude.
        between =
            path |> List.drop 1 |> dropLast
    in
    not (List.any (\p -> Level.blocksSightAt p level) between)


dropLast : List a -> List a
dropLast xs =
    List.take (max 0 (List.length xs - 1)) xs
