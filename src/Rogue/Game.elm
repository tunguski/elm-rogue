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

import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
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
    , inventory : List ItemDef
    , gold : Int
    , weapon : Maybe ItemDef
    , armour : Maybe ItemDef
    , glyph : String
    , color : String
    , fovRadius : Int
    }


{-| The hero's attack power including the worn weapon's bonus. -}
heroDamage : Hero -> Int
heroDamage hero =
    hero.damage + equipBonus .damage hero.weapon


{-| The hero's defense including the worn armour's bonus. -}
heroDefense : Hero -> Int
heroDefense hero =
    hero.defense + equipBonus .defense hero.armour


equipBonus : (Content.EquipBonus -> Int) -> Maybe ItemDef -> Int
equipBonus field maybeItem =
    case maybeItem of
        Just item ->
            case item.kind of
                Content.Equipment _ bonus ->
                    field bonus

                _ ->
                    0

        Nothing ->
            0


{-| A monster in play: a copy of its `EnemyDef` (so the modded stats are the live stats) plus its
current position and HP. -}
type alias Enemy =
    { def : EnemyDef
    , pos : Pos
    , hp : Int
    }


{-| An item lying on the dungeon floor (a copy of its `ItemDef` and where it sits). -}
type alias ItemOnFloor =
    { def : ItemDef
    , pos : Pos
    }


type alias Game =
    { ruleset : Ruleset
    , level : Level
    , rooms : List Room
    , hero : Hero
    , enemies : List Enemy
    , items : List ItemOnFloor
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


{-| Reaching this depth wins the run (the bottom of the dungeon). -}
victoryDepth : Int
victoryDepth =
    8


type Msg
    = Move Dir
    | Descend
    | Wait
    | Use Int
    | Restart
    | NoOp


{-| Start a fresh run at depth 1 from a ruleset, a chosen class and a numeric seed. The class sets
the hero's stats and opening gear (resolved against the ruleset's item list). -}
newGame : Ruleset -> Content.ClassDef -> Int -> Game
newGame ruleset class rawSeed =
    let
        gen =
            Dungeon.generate Dungeon.defaultConfig (Rng.seed rawSeed)

        resolve maybeId =
            Maybe.andThen (\id -> Content.findItem id ruleset) maybeId

        startingInventory =
            List.filterMap (\id -> Content.findItem id ruleset) class.startingItems

        hero =
            { pos = gen.stairsUp
            , hp = class.maxHp
            , maxHp = class.maxHp
            , damage = class.damage
            , defense = class.defense
            , inventory = startingInventory
            , gold = 0
            , weapon = resolve class.startingWeapon
            , armour = resolve class.startingArmour
            , glyph = class.glyph
            , color = class.color
            , fovRadius = class.fovRadius
            }
    in
    enterLevel ruleset 1 0 gen.seed gen hero [ "You enter the dungeon as " ++ withArticle class.name ++ "." ]


withArticle : String -> String
withArticle word =
    let
        starts c =
            String.startsWith c (String.toLower word)
    in
    if starts "a" || starts "e" || starts "i" || starts "o" || starts "u" then
        "an " ++ word

    else
        "a " ++ word


{-| Place the hero on a freshly generated level, spawn its monster population, recompute fog, and keep
the carried-over hero, kill count and log. Shared by `newGame` and descending. -}
enterLevel : Ruleset -> Int -> Int -> Seed -> Generated -> Hero -> List String -> Game
enterLevel ruleset depth kills seed gen hero log =
    let
        heroAt =
            { hero | pos = gen.stairsUp }

        -- One shuffled pool of floor cells feeds both populations so nothing shares a tile.
        ( shuffledSpots, seed1 ) =
            Rng.shuffle (eligibleSpots gen) seed

        enemyCount =
            Content.spawnCountForDepth depth

        ( enemies, seed2 ) =
            spawnEnemies ruleset depth (List.take enemyCount shuffledSpots) seed1

        ( items, seed3 ) =
            spawnItems ruleset depth (List.drop enemyCount shuffledSpots |> List.take (Content.itemCountForDepth depth)) seed2

        vis =
            Fov.compute heroAt.fovRadius heroAt.pos gen.level
    in
    { ruleset = ruleset
    , level = gen.level
    , rooms = gen.rooms
    , hero = heroAt
    , enemies = enemies
    , items = items
    , depth = depth
    , turn = 0
    , kills = kills
    , seed = seed3
    , visible = vis
    , explored = vis
    , log = log
    , stairsDown = gen.stairsDown
    , stairsUp = gen.stairsUp
    , gameOver = False
    , won = False
    }
        |> checkVictory


{-| Arriving at `victoryDepth` ends the run in victory. -}
checkVictory : Game -> Game
checkVictory game =
    if game.depth >= victoryDepth && not game.won then
        { game | won = True, gameOver = True }
            |> addLog ("You reach depth " ++ String.fromInt game.depth ++ " — the bottom of the dungeon. You win!")

    else
        game



-- ENEMY SPAWNING ---------------------------------------------------------------------------------


{-| Drop a depth-appropriate, weight-chosen enemy on each of the given floor cells. Nothing spawns if
the ruleset offers no enemies for this depth. Returns the monsters and the advanced seed. -}
spawnEnemies : Ruleset -> Int -> List Pos -> Seed -> ( List Enemy, Seed )
spawnEnemies ruleset depth spots seed =
    let
        candidates =
            Content.enemiesForDepth depth ruleset
    in
    case candidates of
        [] ->
            ( [], seed )

        ( _, firstDef ) :: _ ->
            List.foldl
                (\pos ( acc, s ) ->
                    let
                        ( def, s2 ) =
                            Rng.pickWeighted firstDef candidates s
                    in
                    ( { def = def, pos = pos, hp = def.maxHp } :: acc, s2 )
                )
                ( [], seed )
                spots


{-| Drop a depth-appropriate, weight-chosen item on each of the given floor cells. -}
spawnItems : Ruleset -> Int -> List Pos -> Seed -> ( List ItemOnFloor, Seed )
spawnItems ruleset depth spots seed =
    let
        candidates =
            Content.itemsForDepth depth ruleset
    in
    case candidates of
        [] ->
            ( [], seed )

        ( _, firstDef ) :: _ ->
            List.foldl
                (\pos ( acc, s ) ->
                    let
                        ( def, s2 ) =
                            Rng.pickWeighted firstDef candidates s
                    in
                    ( { def = def, pos = pos } :: acc, s2 )
                )
                ( [], seed )
                spots


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

            Use index ->
                tryUse index game

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
                endTurn (pickUp (refreshFov { game | hero = moved }))

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



-- ITEMS ------------------------------------------------------------------------------------------


{-| Auto-pick-up anything on the hero's cell (Shattered-Pixel style): gold is spent immediately,
everything else lands in the inventory. Does not consume a turn beyond the move that triggered it. -}
pickUp : Game -> Game
pickUp game =
    let
        ( here, rest ) =
            List.partition (\it -> it.pos == game.hero.pos) game.items
    in
    List.foldl pickUpOne { game | items = rest } here


pickUpOne : ItemOnFloor -> Game -> Game
pickUpOne it game =
    case it.def.kind of
        Content.Consumable (Gold amount) ->
            let
                hero =
                    game.hero
            in
            { game | hero = { hero | gold = hero.gold + amount } }
                |> addLog ("You find " ++ String.fromInt amount ++ " gold.")

        _ ->
            let
                hero =
                    game.hero
            in
            { game | hero = { hero | inventory = hero.inventory ++ [ it.def ] } }
                |> addLog ("You pick up a " ++ it.def.name ++ ".")


{-| Use the inventory item at `index` (0-based): drink a consumable (apply effect, remove it) or wear
a piece of equipment (swap it into its slot, the displaced gear back to the pack). Either way the
monsters then act. Out-of-range indices are ignored (no turn spent). -}
tryUse : Int -> Game -> Game
tryUse index game =
    case nth index game.hero.inventory of
        Nothing ->
            game

        Just def ->
            case def.kind of
                Content.Consumable _ ->
                    let
                        applied =
                            applyEffect def game

                        hero =
                            applied.hero
                    in
                    endTurn { applied | hero = { hero | inventory = removeAt index hero.inventory } }

                Content.Equipment slot _ ->
                    endTurn (equip index slot def game)


{-| Wear `def` (at inventory `index`) in `slot`: pull it from the pack and put whatever was in the
slot back into the pack. -}
equip : Int -> Content.EquipSlot -> ItemDef -> Game -> Game
equip index slot def game =
    let
        hero =
            game.hero

        previous =
            case slot of
                Content.WeaponSlot ->
                    hero.weapon

                Content.ArmourSlot ->
                    hero.armour

        packWithoutNew =
            removeAt index hero.inventory

        pack =
            case previous of
                Just old ->
                    packWithoutNew ++ [ old ]

                Nothing ->
                    packWithoutNew

        equippedHero =
            case slot of
                Content.WeaponSlot ->
                    { hero | inventory = pack, weapon = Just def }

                Content.ArmourSlot ->
                    { hero | inventory = pack, armour = Just def }
    in
    { game | hero = equippedHero }
        |> addLog ("You equip the " ++ def.name ++ ".")


{-| Interpret a consumable's `ItemEffect` on the game — the engine half of the moddable item DSL. A
new effect constructor in `Rogue.Content.ItemEffect` is wired up here. Non-consumables are a no-op. -}
applyEffect : ItemDef -> Game -> Game
applyEffect def game =
    let
        hero =
            game.hero
    in
    case effectOf def of
        HealHp n ->
            { game | hero = { hero | hp = min hero.maxHp (hero.hp + n) } }
                |> addLog ("You drink the " ++ def.name ++ ". (+" ++ String.fromInt n ++ " HP)")

        HealFull ->
            { game | hero = { hero | hp = hero.maxHp } }
                |> addLog ("You drink the " ++ def.name ++ ". You feel restored.")

        MaxHpBonus n ->
            { game | hero = { hero | maxHp = hero.maxHp + n, hp = hero.hp + n } }
                |> addLog ("You drink the " ++ def.name ++ ". (+" ++ String.fromInt n ++ " max HP)")

        DamageBonus n ->
            { game | hero = { hero | damage = hero.damage + n } }
                |> addLog ("You drink the " ++ def.name ++ ". You feel stronger.")

        DefenseBonus n ->
            { game | hero = { hero | defense = hero.defense + n } }
                |> addLog ("You drink the " ++ def.name ++ ". Your skin hardens.")

        Gold amount ->
            { game | hero = { hero | gold = hero.gold + amount } }
                |> addLog ("You gain " ++ String.fromInt amount ++ " gold.")


effectOf : ItemDef -> ItemEffect
effectOf def =
    case def.kind of
        Content.Consumable eff ->
            eff

        _ ->
            HealHp 0



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
            rollDamage (heroDamage game.hero) enemy.def.defense game.seed

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
                rollDamage enemy.def.damage (heroDefense acc.hero) acc.seed

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
            Fov.compute game.hero.fovRadius game.hero.pos game.level
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


nth : Int -> List a -> Maybe a
nth i xs =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i xs)


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs


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
    , glyphs =
        List.map itemGlyph game.items
            ++ List.map enemyGlyph game.enemies
            ++ [ heroGlyph game ]
    , theme = Render.themeForDepth game.depth
    , hud =
        { title = "elm-rouge"
        , region = (Render.themeForDepth game.depth).name
        , depth = game.depth
        , hp = game.hero.hp
        , maxHp = game.hero.maxHp
        , turn = game.turn
        , gold = game.hero.gold
        , weapon = equippedName game.hero.weapon (heroDamage game.hero) "dmg"
        , armour = equippedName game.hero.armour (heroDefense game.hero) "def"
        , inventory = List.map .name game.hero.inventory
        , log = List.take 7 game.log
        , gameOver = game.gameOver
        , won = game.won
        , status = statusLine game
        }
    }


statusLine : Game -> String
statusLine game =
    if game.won then
        "Victory! Press R to play again"

    else if game.gameOver then
        "You have died at depth " ++ String.fromInt game.depth ++ " — press R to restart"

    else if Level.at game.hero.pos game.level == StairsDown then
        "Press > to descend"

    else
        String.fromInt (List.length game.enemies) ++ " monsters · " ++ String.fromInt game.kills ++ " slain"


heroGlyph : Game -> Render.Glyph
heroGlyph game =
    { pos = game.hero.pos
    , char = game.hero.glyph
    , color = game.hero.color
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


{-| A HUD label for an equipped slot: the item's name (or a dash) plus the resulting total stat. -}
equippedName : Maybe ItemDef -> Int -> String -> String
equippedName maybeItem total label =
    let
        prefix =
            case maybeItem of
                Just item ->
                    item.name

                Nothing ->
                    "—"
    in
    prefix ++ " (" ++ label ++ " " ++ String.fromInt total ++ ")"


itemGlyph : ItemOnFloor -> Render.Glyph
itemGlyph item =
    { pos = item.pos
    , char = item.def.glyph
    , color = item.def.color
    , layer = Render.layerItem
    , heavy = False
    }
