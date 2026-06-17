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
        , interactionTests
        , resumeTests
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
        , fuzz (Fuzz.intRange 1 30) "cave floors are also stair-to-stair connected" <|
            \s ->
                let
                    g =
                        Dungeon.generate (Dungeon.configForDepth 5) (Rng.seed (s * 131 + 3))
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


{-| Traversable for solvability: any passable terrain (floor, doors, stairs, water, grass…) plus
secret doors, which the player can search out. Derived from `Tile.isPassable` so it stays correct as
new terrain is added. -}
walkable : Tile -> Bool
walkable tile =
    Tile.isPassable tile || tile == SecretDoor



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


-- INTERACTIONS (driving Msgs through Game.update) ----------------------------


{-| A known 5x3 room — a floor strip walled all round — so movement outcomes are deterministic
(unlike a procedurally generated floor). Built by overriding a real game's level/hero/contents. -}
arena : Game.Game
arena =
    let
        hero =
            newGame.hero
    in
    { newGame
        | level = Level.fromRows [ "#####", "#...#", "#####" ]
        , hero = { hero | pos = { x = 2, y = 1 } }
        , enemies = []
        , items = []
    }


healingPotion : Maybe Content.ItemDef
healingPotion =
    Content.findItem "potion-healing" Default.ruleset


interactionTests : Test
interactionTests =
    describe "Game.update interactions"
        [ test "moving into open floor relocates the hero and spends a turn" <|
            \_ ->
                let
                    next =
                        Game.update (Game.Move Grid.dirE) arena
                in
                Expect.equal ( { x = 3, y = 1 }, arena.turn + 1 ) ( next.hero.pos, next.turn )
        , test "moving into a wall does nothing (no move, no turn spent)" <|
            \_ ->
                let
                    next =
                        Game.update (Game.Move Grid.dirN) arena
                in
                Expect.equal ( arena.hero.pos, arena.turn ) ( next.hero.pos, next.turn )
        , test "waiting spends a turn without moving" <|
            \_ ->
                let
                    next =
                        Game.update Game.Wait arena
                in
                Expect.equal ( arena.hero.pos, arena.turn + 1 ) ( next.hero.pos, next.turn )
        , test "searching spends a turn" <|
            \_ ->
                Expect.equal (arena.turn + 1) (Game.update Game.Search arena).turn
        , test "bumping a monster damages it (or kills it)" <|
            \_ ->
                case List.head newGame.enemies of
                    Nothing ->
                        Expect.fail "expected the starting floor to be populated"

                    Just sample ->
                        let
                            -- Place a single foe just east of the hero in the controlled arena, so the
                            -- lone survivor (if any) is unambiguous even after it acts on its own turn.
                            foe =
                                { sample | pos = { x = 3, y = 1 }, hp = sample.def.maxHp, alerted = True }

                            staged =
                                { arena | enemies = [ foe ] }

                            next =
                                Game.update (Game.Move Grid.dirE) staged
                        in
                        case List.head next.enemies of
                            Just alive ->
                                Expect.equal True (alive.hp < foe.hp)

                            Nothing ->
                                Expect.equal True (next.kills > staged.kills)
        , test "drinking a healing potion restores HP and consumes the item" <|
            \_ ->
                case healingPotion of
                    Nothing ->
                        Expect.fail "ruleset is missing potion-healing"

                    Just potion ->
                        let
                            hero =
                                newGame.hero

                            wounded =
                                { newGame | hero = { hero | hp = 1, maxHp = 40, inventory = [ potion ] } }

                            next =
                                Game.update (Game.Use 0) wounded
                        in
                        Expect.equal ( True, [] ) ( next.hero.hp > 1, List.map .id next.hero.inventory )
        , test "stepping onto an item picks it up" <|
            \_ ->
                case healingPotion of
                    Nothing ->
                        Expect.fail "ruleset is missing potion-healing"

                    Just potion ->
                        let
                            staged =
                                { arena | items = [ { def = potion, pos = { x = 3, y = 1 } } ] }

                            next =
                                Game.update (Game.Move Grid.dirE) staged
                        in
                        Expect.equal (List.length arena.hero.inventory + 1) (List.length next.hero.inventory)
        , test "descending on the down-stairs goes one floor deeper" <|
            \_ ->
                let
                    hero =
                        newGame.hero

                    staged =
                        { newGame | hero = { hero | pos = newGame.stairsDown } }

                    next =
                        Game.update Game.Descend staged
                in
                Expect.equal (newGame.depth + 1) next.depth
        ]


resumeSave : Game.SaveData
resumeSave =
    { depth = 4
    , hp = 17
    , maxHp = 30
    , damage = 7
    , defense = 3
    , gold = 42
    , level = 3
    , xp = 12
    , nutrition = 250
    , fovRadius = 7
    , glyph = "@"
    , color = "#ffe08a"
    , weaponId = Just "short-sword"
    , armourId = Just "leather-armour"
    , ringId = Nothing
    , inventoryIds = [ "potion-healing" ]
    , knownIds = [ "potion-healing" ]
    , seed = 12345
    }


resumedGame : Game.Game
resumedGame =
    Game.resume Default.ruleset resumeSave


resumeTests : Test
resumeTests =
    describe "Game.resume"
        [ test "resumes at the saved depth" <|
            \_ -> Expect.equal 4 resumedGame.depth
        , test "restores the hero's HP" <|
            \_ -> Expect.equal 17 resumedGame.hero.hp
        , test "re-resolves the saved weapon by id" <|
            \_ -> Expect.equal (Just "short-sword") (Maybe.map .id resumedGame.hero.weapon)
        , test "keeps identified items known" <|
            \_ -> Expect.equal True (List.member "potion-healing" (Game.knownItemIds resumedGame))
        ]
