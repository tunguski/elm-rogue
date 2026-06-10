module Rogue.Game exposing
    ( Game
    , Hero
    , Msg(..)
    , newGame
    , update
    , toScene
    , fovRadius
    )

{-| The game state and its pure update — the heart of the engine.

`Game` is a plain record (level, hero, turn/depth counters, the seen/explored fog sets, a message
log, a deterministic seed) and `update` maps a player `Msg` to the next `Game`. Everything is pure
and seed-threaded, so the whole engine runs head-lessly in tests and a replay of the same inputs from
the same seed is identical.

`toScene` projects a `Game` onto the renderer-agnostic `Rogue.Render.Scene`, which is the *only* thing
any renderer ever sees — keeping the simulation and the graphics fully decoupled.

Milestones 4–5 cover hero movement, the turn counter and fog of war; enemies, combat, items and
depth progression build on this same record in later milestones.
-}

import Rogue.Dungeon as Dungeon exposing (Generated)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render as Render exposing (Scene)
import Rogue.Rng as Rng exposing (Seed)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)


fovRadius : Int
fovRadius =
    7


type alias Hero =
    { pos : Pos
    , hp : Int
    , maxHp : Int
    }


type alias Game =
    { level : Level
    , rooms : List Dungeon.Room
    , hero : Hero
    , depth : Int
    , turn : Int
    , seed : Seed
    , visible : Set ( Int, Int )
    , explored : Set ( Int, Int )
    , log : List String
    , stairsDown : Pos
    , stairsUp : Pos
    , gameOver : Bool
    }


type Msg
    = Move Dir
    | Descend
    | Wait
    | NoOp


{-| Start a fresh run at depth 1 from a numeric seed. -}
newGame : Int -> Game
newGame rawSeed =
    let
        gen =
            Dungeon.generate Dungeon.defaultConfig (Rng.seed rawSeed)
    in
    enterLevel 1 gen.seed gen { pos = gen.stairsUp, hp = 20, maxHp = 20 } [ "You descend into the dungeon." ]


{-| Place the hero on a freshly generated level, recompute fog, and keep the carried-over hero and
log. Shared by `newGame` and descending. -}
enterLevel : Int -> Seed -> Generated -> Hero -> List String -> Game
enterLevel depth seed gen hero log =
    let
        heroAt =
            { hero | pos = gen.stairsUp }

        vis =
            Fov.compute fovRadius heroAt.pos gen.level
    in
    { level = gen.level
    , rooms = gen.rooms
    , hero = heroAt
    , depth = depth
    , turn = 0
    , seed = seed
    , visible = vis
    , explored = vis
    , log = log
    , stairsDown = gen.stairsDown
    , stairsUp = gen.stairsUp
    , gameOver = False
    }


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

            NoOp ->
                game


{-| Attempt to step the hero one cell. Walking into a wall costs no turn (standard roguelike feel);
a successful step advances the turn and refreshes fog. -}
tryMove : Dir -> Game -> Game
tryMove dir game =
    let
        target =
            Grid.move game.hero.pos dir
    in
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
        enterLevel (game.depth + 1)
            nextSeedB
            gen
            game.hero
            (("You descend to depth " ++ String.fromInt (game.depth + 1) ++ ".") :: game.log)

    else
        addLog "There are no stairs down here." game


refreshFov : Game -> Game
refreshFov game =
    let
        vis =
            Fov.compute fovRadius game.hero.pos game.level
    in
    { game | visible = vis, explored = Set.union game.explored vis }


endTurn : Game -> Game
endTurn game =
    { game | turn = game.turn + 1 }


addLog : String -> Game -> Game
addLog line game =
    { game | log = line :: game.log }



-- PROJECTION TO A RENDER SCENE -------------------------------------------------------------------


toScene : Game -> Scene
toScene game =
    { level = game.level
    , visible = game.visible
    , explored = game.explored
    , glyphs = [ heroGlyph game.hero ]
    , hud =
        { title = "elm-rouge"
        , depth = game.depth
        , hp = game.hero.hp
        , maxHp = game.hero.maxHp
        , turn = game.turn
        , log = List.take 6 game.log
        , gameOver = game.gameOver
        , status =
            if Level.at game.hero.pos game.level == StairsDown then
                "Press > to descend"

            else
                ""
        }
    }


heroGlyph : Hero -> Render.Glyph
heroGlyph hero =
    { pos = hero.pos
    , char = "@"
    , color = "#ffe08a"
    , layer = Render.layerHero
    , heavy = True
    }
