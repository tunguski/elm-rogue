module Rogue.Game exposing
    ( Game
    , Hero
    , Enemy
    , Msg(..)
    , newGame
    , update
    , toScene
    )

{-| The game state and its pure update — the heart of the engine.

`Game` is a plain record (level, hero, the monsters in play, turn/depth counters, the seen/explored
fog sets, a message log, a deterministic seed and the active `Ruleset`) and `update` maps a player
`Msg` to the next `Game`. Everything is pure and seed-threaded, so the whole engine runs head-lessly
and a replay of the same inputs from the same seed is identical.

The engine reads all its content from the injected `Rogue.Content.Ruleset`: hero stats, the bestiary
and (later) items. That is the moddability contract — swap the ruleset and the same engine plays a
different game.

`toScene` projects a `Game` onto the renderer-agnostic `Rogue.Render.Scene`, the only thing any
renderer ever sees.

Milestones 4–6: hero movement, the turn counter, fog of war and a data-driven monster population.
Combat and AI arrive in M7.
-}

import Rogue.Content as Content exposing (EnemyDef, Ruleset)
import Rogue.Dungeon as Dungeon exposing (Generated, Room)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render as Render exposing (Scene)
import Rogue.Rng as Rng exposing (Seed)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)


type alias Hero =
    { pos : Pos
    , hp : Int
    , maxHp : Int
    , damage : Int
    , defense : Int
    }


{-| A monster in play: a copy of its `EnemyDef` (so the modded stats are the live stats) plus its
current position and HP. -}
type alias Enemy =
    { def : EnemyDef
    , pos : Pos
    , hp : Int
    }


type alias Game =
    { ruleset : Ruleset
    , level : Level
    , rooms : List Room
    , hero : Hero
    , enemies : List Enemy
    , depth : Int
    , turn : Int
    , kills : Int
    , seed : Seed
    , visible : Set ( Int, Int )
    , explored : Set ( Int, Int )
    , log : List String
    , stairsDown : Pos
    , stairsUp : Pos
    , gameOver : Bool
    , won : Bool
    }


{-| How close (Chebyshev) a monster must be, with line of sight, to wake and start hunting. -}
aggroRange : Int
aggroRange =
    8


type Msg
    = Move Dir
    | Descend
    | Wait
    | Restart
    | NoOp


{-| Start a fresh run at depth 1 from a ruleset and a numeric seed. -}
newGame : Ruleset -> Int -> Game
newGame ruleset rawSeed =
    let
        gen =
            Dungeon.generate Dungeon.defaultConfig (Rng.seed rawSeed)

        hero =
            { pos = gen.stairsUp
            , hp = ruleset.hero.maxHp
            , maxHp = ruleset.hero.maxHp
            , damage = ruleset.hero.damage
            , defense = ruleset.hero.defense
            }
    in
    enterLevel ruleset 1 0 gen.seed gen hero [ "You enter the dungeon." ]


{-| Place the hero on a freshly generated level, spawn its monster population, recompute fog, and keep
the carried-over hero, kill count and log. Shared by `newGame` and descending. -}
enterLevel : Ruleset -> Int -> Int -> Seed -> Generated -> Hero -> List String -> Game
enterLevel ruleset depth kills seed gen hero log =
    let
        heroAt =
            { hero | pos = gen.stairsUp }

        ( enemies, seed2 ) =
            spawnEnemies ruleset depth gen seed

        vis =
            Fov.compute ruleset.hero.fovRadius heroAt.pos gen.level
    in
    { ruleset = ruleset
    , level = gen.level
    , rooms = gen.rooms
    , hero = heroAt
    , enemies = enemies
    , depth = depth
    , turn = 0
    , kills = kills
    , seed = seed2
    , visible = vis
    , explored = vis
    , log = log
    , stairsDown = gen.stairsDown
    , stairsUp = gen.stairsUp
    , gameOver = False
    , won = False
    }



-- ENEMY SPAWNING ---------------------------------------------------------------------------------


{-| Seed a floor with monsters: pick `spawnCountForDepth` distinct floor cells (never the start room
or a stair), and at each drop a depth-appropriate, weight-chosen enemy from the ruleset. Returns the
monsters and the advanced seed. Spawns nothing if the ruleset offers no enemies for this depth. -}
spawnEnemies : Ruleset -> Int -> Generated -> Seed -> ( List Enemy, Seed )
spawnEnemies ruleset depth gen seed =
    let
        candidates =
            Content.enemiesForDepth depth ruleset
    in
    if List.isEmpty candidates then
        ( [], seed )

    else
        let
            spots =
                eligibleSpots gen

            ( shuffled, seed1 ) =
                Rng.shuffle spots seed

            chosen =
                List.take (Content.spawnCountForDepth depth) shuffled
        in
        List.foldl
            (\pos ( acc, s ) ->
                case candidates of
                    ( _, firstDef ) :: _ ->
                        let
                            ( def, s2 ) =
                                Rng.pickWeighted firstDef candidates s
                        in
                        ( { def = def, pos = pos, hp = def.maxHp } :: acc, s2 )

                    [] ->
                        ( acc, s )
            )
            ( [], seed1 )
            chosen


{-| Floor cells eligible to host a monster: any floor tile outside the first (start) room and not on
a stair. -}
eligibleSpots : Generated -> List Pos
eligibleSpots gen =
    let
        startRoom =
            List.head gen.rooms
    in
    Level.positions gen.level
        |> List.filter
            (\p ->
                (Level.at p gen.level == Floor)
                    && not (inRoom p startRoom)
                    && p /= gen.stairsDown
                    && p /= gen.stairsUp
            )


inRoom : Pos -> Maybe Room -> Bool
inRoom p maybeRoom =
    case maybeRoom of
        Nothing ->
            False

        Just r ->
            p.x >= r.x - 1 && p.x <= r.x + r.w && p.y >= r.y - 1 && p.y <= r.y + r.h



-- UPDATE -----------------------------------------------------------------------------------------


update : Msg -> Game -> Game
update msg game =
    if game.gameOver then
        game

    else
        case msg of
            Move dir ->
                tryMove dir game

            Descend ->
                tryDescend game

            Wait ->
                endTurn game

            Restart ->
                -- The shell (Main) owns reseeding a new run; inside a game it's a no-op.
                game

            NoOp ->
                game


{-| The hero's intent for one cell: bumping a monster attacks it, an open cell is a step, a wall is a
no-op (costs no turn). A turn-consuming action is followed by every monster taking its turn. -}
tryMove : Dir -> Game -> Game
tryMove dir game =
    let
        target =
            Grid.move game.hero.pos dir
    in
    case enemyAt target game of
        Just enemy ->
            endTurn (heroAttack enemy game)

        Nothing ->
            if Level.isPassableAt target game.level then
                let
                    hero =
                        game.hero

                    moved =
                        { hero | pos = target }
                in
                endTurn (refreshFov { game | hero = moved })

            else
                game


tryDescend : Game -> Game
tryDescend game =
    if Level.at game.hero.pos game.level == StairsDown then
        let
            ( nextSeedA, nextSeedB ) =
                Rng.split game.seed

            gen =
                Dungeon.generate Dungeon.defaultConfig nextSeedA
        in
        enterLevel game.ruleset
            (game.depth + 1)
            game.kills
            nextSeedB
            gen
            game.hero
            (("You descend to depth " ++ String.fromInt (game.depth + 1) ++ ".") :: game.log)

    else
        addLog "There are no stairs down here." game



-- COMBAT -----------------------------------------------------------------------------------------


{-| Damage is `attack − defense` with ±1 of scatter, never below 1, drawn from the game seed. -}
rollDamage : Int -> Int -> Seed -> ( Int, Seed )
rollDamage attack defense seed =
    let
        base =
            attack - defense
    in
    Rng.range (max 1 (base - 1)) (max 1 (base + 1)) seed


{-| The hero strikes a monster; a lethal blow removes it and counts a kill. -}
heroAttack : Enemy -> Game -> Game
heroAttack enemy game =
    let
        ( dmg, seed1 ) =
            rollDamage game.hero.damage enemy.def.defense game.seed

        remaining =
            enemy.hp - dmg
    in
    if remaining <= 0 then
        { game
            | enemies = List.filter (\e -> e.pos /= enemy.pos) game.enemies
            , seed = seed1
            , kills = game.kills + 1
        }
            |> addLog ("You kill the " ++ enemy.def.name ++ ".")

    else
        { game
            | enemies = updateEnemyAt enemy.pos (\e -> { e | hp = remaining }) game.enemies
            , seed = seed1
        }
            |> addLog ("You hit the " ++ enemy.def.name ++ " (" ++ String.fromInt dmg ++ ").")



-- MONSTER TURN -----------------------------------------------------------------------------------


{-| Every living monster acts once, in list order, after a hero action that consumed a turn. Each
either attacks the adjacent hero, steps toward a hero it can see, or idles. Positions are threaded
through an `occupied` set so monsters never stack. -}
enemiesTurn : Game -> Game
enemiesTurn game =
    if game.gameOver then
        game

    else
        let
            occupied0 =
                Set.fromList (( game.hero.pos.x, game.hero.pos.y ) :: List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies)

            ( newEnemiesRev, acc ) =
                List.foldl stepEnemy ( [], { hero = game.hero, seed = game.seed, log = game.log, occupied = occupied0, level = game.level } ) game.enemies
        in
        checkHeroDeath
            { game
                | enemies = List.reverse newEnemiesRev
                , hero = acc.hero
                , seed = acc.seed
                , log = acc.log
            }


type alias TurnAcc =
    { hero : Hero
    , seed : Seed
    , log : List String
    , occupied : Set ( Int, Int )
    , level : Level
    }


stepEnemy : Enemy -> ( List Enemy, TurnAcc ) -> ( List Enemy, TurnAcc )
stepEnemy enemy ( done, acc ) =
    let
        heroPos =
            acc.hero.pos

        adjacent =
            Grid.chebyshev enemy.pos heroPos == 1

        aware =
            Grid.chebyshev enemy.pos heroPos <= aggroRange && Fov.visibleFrom enemy.pos heroPos acc.level
    in
    if adjacent then
        let
            ( dmg, seed1 ) =
                rollDamage enemy.def.damage acc.hero.defense acc.seed

            hero =
                acc.hero

            hurt =
                { hero | hp = hero.hp - dmg }
        in
        ( enemy :: done
        , { acc
            | hero = hurt
            , seed = seed1
            , log = ("The " ++ enemy.def.name ++ " hits you (" ++ String.fromInt dmg ++ ").") :: acc.log
          }
        )

    else if aware then
        case stepToward enemy.pos heroPos acc.level acc.occupied of
            Just next ->
                ( { enemy | pos = next } :: done
                , { acc
                    | occupied =
                        acc.occupied
                            |> Set.remove ( enemy.pos.x, enemy.pos.y )
                            |> Set.insert ( next.x, next.y )
                  }
                )

            Nothing ->
                ( enemy :: done, acc )

    else
        ( enemy :: done, acc )


{-| Greedy chase: of the passable, unoccupied neighbours, the one that most reduces Chebyshev
distance to the target. `Nothing` if boxed in. -}
stepToward : Pos -> Pos -> Level -> Set ( Int, Int ) -> Maybe Pos
stepToward from to level occupied =
    Grid.eightDirs
        |> List.map (Grid.move from)
        |> List.filter (\p -> Level.isPassableAt p level && not (Set.member ( p.x, p.y ) occupied))
        |> minimumBy (\p -> Grid.chebyshev p to)


checkHeroDeath : Game -> Game
checkHeroDeath game =
    if game.hero.hp <= 0 && not game.gameOver then
        { game | gameOver = True } |> addLog "You die. Press R to restart."

    else
        game


enemyAt : Pos -> Game -> Maybe Enemy
enemyAt p game =
    listFind (\e -> e.pos == p) game.enemies


updateEnemyAt : Pos -> (Enemy -> Enemy) -> List Enemy -> List Enemy
updateEnemyAt p f enemies =
    List.map
        (\e ->
            if e.pos == p then
                f e

            else
                e
        )
        enemies


refreshFov : Game -> Game
refreshFov game =
    let
        vis =
            Fov.compute game.ruleset.hero.fovRadius game.hero.pos game.level
    in
    { game | visible = vis, explored = Set.union game.explored vis }


{-| Close out a turn-consuming hero action: run the monsters, then tick the counter. -}
endTurn : Game -> Game
endTurn game =
    let
        afterMonsters =
            enemiesTurn game
    in
    { afterMonsters | turn = afterMonsters.turn + 1 }


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl (\x best -> if f x < f best then x else best) first rest)


addLog : String -> Game -> Game
addLog line game =
    { game | log = line :: game.log }


listFind : (a -> Bool) -> List a -> Maybe a
listFind pred xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if pred x then
                Just x

            else
                listFind pred rest



-- PROJECTION TO A RENDER SCENE -------------------------------------------------------------------


toScene : Game -> Scene
toScene game =
    { level = game.level
    , visible = game.visible
    , explored = game.explored
    , glyphs = heroGlyph game :: List.map enemyGlyph game.enemies
    , hud =
        { title = "elm-rouge"
        , depth = game.depth
        , hp = game.hero.hp
        , maxHp = game.hero.maxHp
        , turn = game.turn
        , log = List.take 7 game.log
        , gameOver = game.gameOver
        , status = statusLine game
        }
    }


statusLine : Game -> String
statusLine game =
    if game.gameOver then
        "You have died at depth " ++ String.fromInt game.depth ++ " — press R to restart"

    else if Level.at game.hero.pos game.level == StairsDown then
        "Press > to descend"

    else
        String.fromInt (List.length game.enemies) ++ " monsters · " ++ String.fromInt game.kills ++ " slain"


heroGlyph : Game -> Render.Glyph
heroGlyph game =
    { pos = game.hero.pos
    , char = game.ruleset.hero.glyph
    , color = game.ruleset.hero.color
    , layer = Render.layerHero
    , heavy = True
    }


enemyGlyph : Enemy -> Render.Glyph
enemyGlyph enemy =
    { pos = enemy.pos
    , char = enemy.def.glyph
    , color = enemy.def.color
    , layer = Render.layerActor
    , heavy = False
    }
