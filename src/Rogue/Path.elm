module Rogue.Path exposing (firstStep)

{-| Breadth-first pathfinding on the dungeon grid.

`firstStep` returns the first cell of a shortest 8-directional path from `start` to `goal` over
passable, unoccupied tiles — what a hunting monster needs to actually round corners and thread
corridors toward the hero, instead of the greedy "step that reduces distance" that gets stuck on
walls. The `goal` cell itself is always treated as reachable (it holds the hero), so the final step
lands as a bump-attack.

Pure and self-contained: BFS with a parent map, reconstructing just the one step the caller needs.
-}

import Dict exposing (Dict)
import Rogue.Grid as Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Set exposing (Set)


{-| The first step from `start` toward `goal`, or `Nothing` if no path exists. `occupied` cells (other
actors) are impassable except the goal. -}
firstStep : Level -> Set ( Int, Int ) -> Pos -> Pos -> Maybe Pos
firstStep level occupied start goal =
    let
        parents =
            explore level occupied goal [ start ] (Set.singleton ( start.x, start.y )) Dict.empty
    in
    reconstruct start ( goal.x, goal.y ) parents


explore : Level -> Set ( Int, Int ) -> Pos -> List Pos -> Set ( Int, Int ) -> Dict ( Int, Int ) ( Int, Int ) -> Dict ( Int, Int ) ( Int, Int )
explore level occupied goal frontier visited parents =
    case frontier of
        [] ->
            parents

        _ ->
            if Set.member ( goal.x, goal.y ) visited then
                parents

            else
                let
                    walkable nb =
                        nb
                            == goal
                            || (Level.isPassableAt nb level && not (Set.member ( nb.x, nb.y ) occupied))

                    expand cur ( nf0, v0, p0 ) =
                        List.foldl
                            (\nb ( nf, v, p ) ->
                                if walkable nb && not (Set.member ( nb.x, nb.y ) v) then
                                    ( nb :: nf, Set.insert ( nb.x, nb.y ) v, Dict.insert ( nb.x, nb.y ) ( cur.x, cur.y ) p )

                                else
                                    ( nf, v, p )
                            )
                            ( nf0, v0, p0 )
                            (List.map (Grid.move cur) Grid.eightDirs)

                    ( nf, v, p ) =
                        List.foldl expand ( [], visited, parents ) frontier
                in
                explore level occupied goal nf v p


reconstruct : Pos -> ( Int, Int ) -> Dict ( Int, Int ) ( Int, Int ) -> Maybe Pos
reconstruct start cur parents =
    case Dict.get cur parents of
        Nothing ->
            Nothing

        Just parent ->
            if parent == ( start.x, start.y ) then
                Just { x = Tuple.first cur, y = Tuple.second cur }

            else
                reconstruct start parent parents
