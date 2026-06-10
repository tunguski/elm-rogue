module RogueTest exposing (suite)

{-| The elm-rogue test suite: pure, browser-free checks of the deterministic engine. Run with

    elm test test/RogueTest.elm \
      src/Rogue/Rng.elm src/Rogue/Grid.elm src/Rogue/Tile.elm src/Rogue/Level.elm \
      src/Rogue/Fov.elm src/Rogue/Path.elm src/Rogue/Dungeon.elm src/Rogue/Content.elm \
      src/Rogue/Render.elm src/Rogue/Game.elm src/Mod/Default.elm

Because the whole engine is seed-threaded and effect-free, every behaviour is checked without a
browser. The flagship property is **floor connectivity**: from a generated floor's up-stairs you can
always reach the down-stairs over passable terrain (ignoring the optional locked vault).
-}

import Expect
import Fuzz
import Mod.Default as Default
import Rogue.Content as Content
import Rogue.Dungeon as Dungeon
import Rogue.Game as Game
import Rogue.Grid as Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Rng as Rng
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "elm-rogue"
        [ rngTests
        , gridTests
        , levelTests
        , dungeonTests
        , gameTests
        ]



-- RNG ------------------------------------------------------------------------


rngTests : Test
rngTests =
    describe "Rng"
        [ test "int n lands in [0, n)" <|
            \_ ->
                let
                    ( v, _ ) =
                        Rng.int 10 (Rng.seed 42)
                in
                Expect.equal True (v >= 0 && v < 10)
        , fuzz (Fuzz.intRange 1 1000) "range lo hi lands in [lo, hi]" <|
            \n ->
                let
                    ( v, _ ) =
                        Rng.range 5 (5 + n) (Rng.seed n)
                in
                Expect.equal True (v >= 5 && v <= 5 + n)
        , test "same seed is deterministic" <|
            \_ ->
                Expect.equal (Rng.int 100 (Rng.seed 7)) (Rng.int 100 (Rng.seed 7))
        , test "shuffle preserves length" <|
            \_ ->
                let
                    ( shuffled, _ ) =
                        Rng.shuffle [ 1, 2, 3, 4, 5 ] (Rng.seed 3)
                in
                Expect.equal 5 (List.length shuffled)
        ]



-- GRID -----------------------------------------------------------------------


gridTests : Test
gridTests =
    describe "Grid"
        [ test "chebyshev is the king-move distance" <|
            \_ -> Expect.equal 3 (Grid.chebyshev { x = 0, y = 0 } { x = 3, y = 2 })
        , test "manhattan is taxicab distance" <|
            \_ -> Expect.equal 5 (Grid.manhattan { x = 0, y = 0 } { x = 3, y = 2 })
        , test "a line starts at a and ends at b" <|
            \_ ->
                let
                    pts =
                        Grid.line { x = 0, y = 0 } { x = 4, y = 2 }
                in
                Expect.equal ( Just { x = 0, y = 0 }, Just { x = 4, y = 2 } )
                    ( List.head pts, List.head (List.reverse pts) )
        ]



-- LEVEL ----------------------------------------------------------------------


levelTests : Test
levelTests =
    describe "Level"
        [ test "fromRows reads tiles by glyph" <|
            \_ ->
                let
                    lvl =
                        Level.fromRows [ "##", "#." ]
                in
                Expect.equal ( Wall, Floor )
                    ( Level.at { x = 0, y = 0 } lvl, Level.at { x = 1, y = 1 } lvl )
        , test "out of bounds reads Empty" <|
            \_ ->
                Expect.equal Empty (Level.at { x = 9, y = 9 } (Level.fromRows [ "##" ]))
        ]



-- DUNGEON --------------------------------------------------------------------


gen : Int -> Dungeon.Generated
gen s =
    Dungeon.generate Dungeon.defaultConfig (Rng.seed s)


dungeonTests : Test
dungeonTests =
    describe "Dungeon"
        [ test "up and down stairs are distinct" <|
            \_ ->
                let
                    g =
                        gen 12345
                in
                Expect.notEqual g.stairsUp g.stairsDown
        , test "generation is deterministic" <|
            \_ ->
                Expect.equal (gen 999).stairsDown (gen 999).stairsDown
        , fuzz (Fuzz.intRange 1 50) "down-stairs are reachable from up-stairs" <|
            \s ->
                let
                    g =
                        gen (s * 101 + 7)
                in
                Expect.equal True (reachable g.level g.stairsUp g.stairsDown)
        ]


{-| BFS reachability over walkable terrain (treating doors — even secret ones — as passable, since
the player can open/find them; only walls, locked doors and the void block). -}
reachable : Level -> Pos -> Pos -> Bool
reachable level start goal =
    bfs level goal [ start ] (Set.singleton ( start.x, start.y ))


bfs : Level -> Pos -> List Pos -> Set ( Int, Int ) -> Bool
bfs level goal frontier visited =
    case frontier of
        [] ->
            False

        _ ->
            if Set.member ( goal.x, goal.y ) visited then
                True

            else
                let
                    ( nextFrontier, nextVisited ) =
                        List.foldl
                            (\cur ( fr, vis ) ->
                                List.foldl
                                    (\nb ( fr2, vis2 ) ->
                                        if walkable (Level.at nb level) && not (Set.member ( nb.x, nb.y ) vis2) then
                                            ( nb :: fr2, Set.insert ( nb.x, nb.y ) vis2 )

                                        else
                                            ( fr2, vis2 )
                                    )
                                    ( fr, vis )
                                    (Grid.neighbors4 cur)
                            )
                            ( [], visited )
                            frontier
                in
                if List.isEmpty nextFrontier then
                    Set.member ( goal.x, goal.y ) visited

                else
                    bfs level goal nextFrontier nextVisited


walkable : Tile -> Bool
walkable tile =
    case tile of
        Floor ->
            True

        Door ->
            True

        OpenDoor ->
            True

        SecretDoor ->
            True

        StairsDown ->
            True

        StairsUp ->
            True

        _ ->
            False



-- GAME -----------------------------------------------------------------------


newGame : Game.Game
newGame =
    Game.newGame Default.ruleset (Content.defaultClass Default.ruleset) 20260610


gameTests : Test
gameTests =
    describe "Game.newGame"
        [ test "starts at depth 1" <|
            \_ -> Expect.equal 1 newGame.depth
        , test "places the hero on the up-stairs" <|
            \_ -> Expect.equal newGame.stairsUp newGame.hero.pos
        , test "the hero starts at full HP" <|
            \_ -> Expect.equal newGame.hero.maxHp newGame.hero.hp
        , test "the floor is populated with monsters" <|
            \_ -> Expect.equal True (not (List.isEmpty newGame.enemies))
        ]
