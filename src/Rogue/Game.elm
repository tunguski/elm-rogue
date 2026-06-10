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

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
import Rogue.Dungeon as Dungeon exposing (Generated, Room)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Path as Path
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
    , ring : Maybe ItemDef
    , glyph : String
    , color : String
    , fovRadius : Int
    , statuses : List Status
    , level : Int
    , xp : Int
    , nutrition : Int
    }


maxNutrition : Int
maxNutrition =
    450


{-| A timed condition on the hero. `magnitude` is HP per turn (drained for `Poison`/`Burn`, restored
for `Regen`); `turns` is how many turns remain. -}
type StatusKind
    = Poison
    | Burn
    | Regen
    | Lit
    | Paralyzed
    | Hasted
    | Slowed


type alias Status =
    { kind : StatusKind
    , magnitude : Int
    , turns : Int
    }


statusLabel : Status -> String
statusLabel status =
    let
        name =
            case status.kind of
                Poison ->
                    "Poison"

                Burn ->
                    "Burn"

                Regen ->
                    "Regen"

                Lit ->
                    "Light"

                Paralyzed ->
                    "Paralyzed"

                Hasted ->
                    "Haste"

                Slowed ->
                    "Slow"
    in
    name ++ " (" ++ String.fromInt status.turns ++ ")"


hasStatus : StatusKind -> Hero -> Bool
hasStatus kind hero =
    List.any (\s -> s.kind == kind) hero.statuses


{-| The hero's attack power including the worn weapon's and ring's bonuses. -}
heroDamage : Hero -> Int
heroDamage hero =
    hero.damage + equipBonus .damage hero.weapon + equipBonus .damage hero.ring


{-| The hero's defense including the worn armour's and ring's bonuses. -}
heroDefense : Hero -> Int
heroDefense hero =
    hero.defense + equipBonus .defense hero.armour + equipBonus .defense hero.ring


equipBonus : (Content.EquipBonus -> Int) -> Maybe ItemDef -> Int
equipBonus field maybeItem =
    case maybeItem of
        Just item ->
            case item.kind of
                Content.Equipment _ bonus ->
                    field bonus + bonus.plus

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
    , alerted : Bool
    , fleeing : Bool
    , statuses : List Status
    }


{-| An item lying on the dungeon floor (a copy of its `ItemDef` and where it sits). -}
type alias ItemOnFloor =
    { def : ItemDef
    , pos : Pos
    }


{-| A transient floating combat number, shown for the frame after the action that produced it. -}
type alias Popup =
    { pos : Pos
    , text : String
    , color : String
    }


{-| An item for sale in a shop: stepping onto its cell buys it if the hero can afford the price. -}
type alias ShopEntry =
    { def : ItemDef
    , pos : Pos
    , price : Int
    }


{-| Floors that host a shop. -}
shopDepth : Int -> Bool
shopDepth depth =
    depth == 3 || depth == 6


{-| A floor hazard. Hidden until it triggers (you step on it) or you `Search` it out. -}
type TrapKind
    = DartTrap
    | PoisonTrap
    | TeleportTrap
    | ParalysisTrap


type alias Trap =
    { pos : Pos
    , kind : TrapKind
    , revealed : Bool
    }


{-| Per-run identification of potions. `looks` maps each potion id to the random appearance it wears
this run (e.g. "murky"); `known` is the set of potion ids the hero has identified by drinking. An
unknown potion shows its appearance; a known one shows its true name. -}
type alias Idents =
    { known : Set String
    , looks : Dict String Appearance
    }


type alias Appearance =
    { adjective : String
    , color : String
    }


type alias Game =
    { ruleset : Ruleset
    , level : Level
    , rooms : List Room
    , hero : Hero
    , enemies : List Enemy
    , items : List ItemOnFloor
    , shop : List ShopEntry
    , popups : List Popup
    , traps : List Trap
    , idents : Idents
    , depth : Int
    , turn : Int
    , tempo : Int
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


{-| Deep floors are gloomy, shrinking sight; a lit torch (a `Lit` status) pushes it back out. The
effective view radius is the hero's base FOV minus the floor's darkness plus any active light, floored
at 2. -}
fovRadiusFor : Hero -> Int -> Int
fovRadiusFor hero depth =
    let
        darkPenalty =
            if depth >= 5 then
                3

            else
                0

        litBonus =
            hero.statuses
                |> List.filter (\s -> s.kind == Lit)
                |> List.map .magnitude
                |> List.maximum
                |> Maybe.withDefault 0
    in
    max 2 (hero.fovRadius - darkPenalty + litBonus)


{-| Reaching this depth wins the run (the bottom of the dungeon). -}
victoryDepth : Int
victoryDepth =
    8


type Msg
    = Move Dir
    | Descend
    | Wait
    | Use Int
    | Search
    | Fire
    | ThrowAt Pos
    | Brew
    | Drop Int
    | Restart
    | NoOp


{-| Start a fresh run at depth 1 from a ruleset, a chosen class and a numeric seed. The class sets
the hero's stats and opening gear (resolved against the ruleset's item list). -}
newGame : Ruleset -> Content.ClassDef -> Int -> Game
newGame ruleset class rawSeed =
    let
        gen =
            Dungeon.generate (Dungeon.configForDepth 1) (Rng.seed rawSeed)

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
            , ring = Nothing
            , glyph = class.glyph
            , color = class.color
            , fovRadius = class.fovRadius
            , statuses = []
            , level = 1
            , xp = 0
            , nutrition = 400
            }
    in
    let
        ( looks, seedA ) =
            assignLooks ruleset gen.seed

        idents =
            { known = Set.empty, looks = looks }
    in
    enterLevel ruleset 1 0 idents seedA gen hero [ "You enter the dungeon as " ++ withArticle class.name ++ "." ]


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
enterLevel : Ruleset -> Int -> Int -> Idents -> Seed -> Generated -> Hero -> List String -> Game
enterLevel ruleset depth kills idents seed gen hero log =
    let
        heroAt =
            { hero | pos = gen.stairsUp }

        -- One shuffled pool of floor cells feeds both populations so nothing shares a tile.
        ( shuffledSpots, seed1 ) =
            Rng.shuffle (eligibleSpots gen) seed

        -- Scale populations with the floor's actual size so big floors / caves aren't empty.
        floorCount =
            List.length shuffledSpots

        enemyCount =
            min (floorCount // 5) (Content.spawnCountForDepth depth + floorCount // 45)

        ( enemies, seed2 ) =
            spawnEnemies ruleset depth (List.take enemyCount shuffledSpots) seed1

        itemCount =
            Content.itemCountForDepth depth + floorCount // 120

        ( items, seed3 ) =
            spawnItems ruleset depth (List.drop enemyCount shuffledSpots |> List.take itemCount) seed2

        trapCount =
            trapCountForDepth depth

        ( traps, seed4 ) =
            spawnTraps depth (List.drop (enemyCount + itemCount) shuffledSpots |> List.take trapCount) seed3

        keySpots =
            List.drop (enemyCount + itemCount + trapCount) shuffledSpots
                |> List.filter (\p -> not (List.member p gen.vaultCells))

        ( vaultItems, seed5 ) =
            spawnVault ruleset depth gen keySpots seed4

        ( featureEnemies, featureItems, seed6 ) =
            spawnFeatures ruleset depth (floorFeatures gen) seed5

        bossEnemy =
            case Content.bossForDepth depth ruleset of
                Just def ->
                    [ { def = def, pos = bossSpot gen, hp = def.maxHp, alerted = False, fleeing = False, statuses = [] } ]

                Nothing ->
                    []

        bossLog =
            if List.isEmpty bossEnemy then
                log

            else
                "A powerful presence guards this floor!" :: log

        ( shop, seed7 ) =
            if shopDepth depth then
                buildShop ruleset depth gen seed6

            else
                ( [], seed6 )

        amuletItems =
            if depth >= victoryDepth then
                case Content.findItem "amulet" ruleset of
                    Just amuletDef ->
                        [ { def = amuletDef, pos = bossSpot gen } ]

                    Nothing ->
                        []

            else
                []

        vis =
            Fov.compute (fovRadiusFor heroAt depth) heroAt.pos gen.level
    in
    { ruleset = ruleset
    , level = gen.level
    , rooms = gen.rooms
    , hero = heroAt
    , enemies = enemies ++ featureEnemies ++ bossEnemy
    , items = items ++ vaultItems ++ featureItems ++ amuletItems
    , shop = shop
    , popups = []
    , traps = traps
    , idents = idents
    , depth = depth
    , turn = 0
    , tempo = 0
    , kills = kills
    , seed = seed7
    , visible = vis
    , explored = vis
    , log = bossLog
    , stairsDown = gen.stairsDown
    , stairsUp = gen.stairsUp
    , gameOver = False
    , won = False
    }
        |> checkVictory


{-| The deepest floor carries the Amulet of Yendor; the run is won by claiming it (see `pickUpOne`),
not merely by arriving. -}
checkVictory : Game -> Game
checkVictory game =
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
                    ( { def = def, pos = pos, hp = def.maxHp, alerted = False, fleeing = False, statuses = [] } :: acc, s2 )
                )
                ( [], seed )
                spots


trapCountForDepth : Int -> Int
trapCountForDepth depth =
    min 6 (1 + depth)


{-| Seed hidden traps on the given floor cells; their kind is rolled from the depth (teleport traps
only appear deeper). Returns the traps and the advanced seed. -}
spawnTraps : Int -> List Pos -> Seed -> ( List Trap, Seed )
spawnTraps depth spots seed =
    List.foldl
        (\pos ( acc, s ) ->
            let
                ( kind, s2 ) =
                    rollTrapKind depth s
            in
            ( { pos = pos, kind = kind, revealed = False } :: acc, s2 )
        )
        ( [], seed )
        spots


rollTrapKind : Int -> Seed -> ( TrapKind, Seed )
rollTrapKind depth seed =
    let
        candidates =
            [ ( 5, DartTrap ), ( 4, PoisonTrap ) ]
                ++ (if depth >= 3 then
                        [ ( 3, TeleportTrap ), ( 3, ParalysisTrap ) ]

                    else
                        []
                   )
    in
    Rng.pickWeighted DartTrap candidates seed


{-| Stock a shop in a middle room: a handful of depth-appropriate items, each with a gold price. -}
buildShop : Ruleset -> Int -> Generated -> Seed -> ( List ShopEntry, Seed )
buildShop ruleset depth gen seed =
    let
        candidates =
            Content.itemsForDepth depth ruleset
                |> List.filter (\( _, def ) -> def.id /= "gold" && def.id /= "amulet" && def.id /= "key")

        room =
            gen.rooms |> List.drop 1 |> List.head

        cells =
            case room of
                Just r ->
                    Dungeon.roomCells r |> List.filter (\p -> Level.at p gen.level == Floor) |> List.take 5

                Nothing ->
                    []
    in
    case candidates of
        ( _, firstDef ) :: _ ->
            List.foldl
                (\pos ( acc, s ) ->
                    let
                        ( def, s2 ) =
                            Rng.pickWeighted firstDef candidates s
                    in
                    ( { def = def, pos = pos, price = priceFor depth def } :: acc, s2 )
                )
                ( [], seed )
                cells

        [] ->
            ( [], seed )


{-| A shop price by item kind, scaled mildly with depth. -}
priceFor : Int -> ItemDef -> Int
priceFor depth def =
    let
        base =
            case def.kind of
                Content.Equipment _ _ ->
                    60

                Content.Wand _ ->
                    80

                Content.Consumable _ ->
                    30

                Content.Key ->
                    20
    in
    base + depth * 5


{-| Standing on a shop item buys it if the hero has the gold; otherwise it stays for sale. -}
tryBuy : Game -> Game
tryBuy game =
    case listFind (\e -> e.pos == game.hero.pos) game.shop of
        Nothing ->
            game

        Just entry ->
            let
                hero =
                    game.hero
            in
            if hero.gold >= entry.price then
                { game
                    | hero = { hero | gold = hero.gold - entry.price, inventory = hero.inventory ++ [ entry.def ] }
                    , shop = List.filter (\e -> e.pos /= entry.pos) game.shop
                }
                    |> addLog ("Bought " ++ displayName game.idents entry.def ++ " for " ++ String.fromInt entry.price ++ " gold.")

            else
                addLog (displayName game.idents entry.def ++ " costs " ++ String.fromInt entry.price ++ " gold — you can't afford it.") game


{-| Where the floor's boss stands: a passable cell beside the down-stairs (so it guards the descent),
falling back to the stairs themselves. -}
bossSpot : Generated -> Pos
bossSpot gen =
    Grid.neighbors8 gen.stairsDown
        |> List.filter (\p -> Level.isPassableAt p gen.level && p /= gen.stairsUp)
        |> List.head
        |> Maybe.withDefault gen.stairsDown


{-| Populate the tagged special rooms: a treasure room gets extra loot, a nest gets an extra monster
pack. Returns the added enemies, items, and advanced seed. -}
{-| Restrict each feature's cells to the actually-carved floor (shaped rooms leave wall corners and
pillars), so feature loot/monsters never land inside a wall. -}
floorFeatures : Generated -> List Dungeon.Feature
floorFeatures gen =
    List.map
        (\f -> { f | cells = List.filter (\p -> Level.at p gen.level == Floor) f.cells })
        gen.features


spawnFeatures : Ruleset -> Int -> List Dungeon.Feature -> Seed -> ( List Enemy, List ItemOnFloor, Seed )
spawnFeatures ruleset depth features seed =
    List.foldl
        (\feature ( accE, accI, s ) ->
            case feature.kind of
                Dungeon.Treasure ->
                    let
                        ( items, s2 ) =
                            spawnItems ruleset depth (List.take 3 feature.cells) s
                    in
                    ( accE, items ++ accI, s2 )

                Dungeon.Library ->
                    let
                        scrolls =
                            ruleset.items
                                |> List.filter (\i -> String.startsWith "scroll" i.id)

                        ( items, s2 ) =
                            spawnFrom scrolls depth (List.take 3 feature.cells) s
                    in
                    ( accE, items ++ accI, s2 )

                Dungeon.Pool ->
                    let
                        ( items, s2 ) =
                            spawnItems ruleset depth (List.take 2 feature.cells) s
                    in
                    ( accE, items ++ accI, s2 )

                Dungeon.Nest ->
                    let
                        ( enemies, s2 ) =
                            spawnEnemies ruleset depth (List.take 4 feature.cells) s
                    in
                    ( enemies ++ accE, accI, s2 )

                Dungeon.Pit ->
                    ( accE, accI, s )
        )
        ( [], [], seed )
        features


{-| Place one of the given item defs (already filtered, e.g. scrolls) on each cell, weighted by spawn
weight. -}
spawnFrom : List ItemDef -> Int -> List Pos -> Seed -> ( List ItemOnFloor, Seed )
spawnFrom defs _ cells seed =
    case defs of
        [] ->
            ( [], seed )

        first :: _ ->
            List.foldl
                (\pos ( acc, s ) ->
                    let
                        ( def, s2 ) =
                            Rng.pickWeighted first (List.map (\d -> ( max 1 d.spawnWeight, d )) defs) s
                    in
                    ( { def = def, pos = pos } :: acc, s2 )
                )
                ( [], seed )
                cells


{-| Stock a generated vault (if any): a couple of bonus items inside, plus a guaranteed key dropped on
a non-vault floor cell so the locked door can always be opened. No vault → nothing. -}
spawnVault : Ruleset -> Int -> Generated -> List Pos -> Seed -> ( List ItemOnFloor, Seed )
spawnVault ruleset depth gen keySpots seed =
    case ( gen.vaultDoor, Content.findItem "key" ruleset, List.head keySpots ) of
        ( Just _, Just keyDef, Just keyPos ) ->
            let
                ( bonus, seed1 ) =
                    spawnItems ruleset depth (List.take 2 gen.vaultCells) seed
            in
            ( { def = keyDef, pos = keyPos } :: bonus, seed1 )

        _ ->
            ( [], seed )


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
update rawMsg rawGame =
    let
        -- Each new action starts with a clean set of floating numbers.
        game =
            if isActionMsg rawMsg then
                { rawGame | popups = [] }

            else
                rawGame

        msg =
            rawMsg
    in
    if game.gameOver then
        game

    else if hasStatus Paralyzed game.hero && isActionMsg msg then
        -- Paralysed: the action is lost but time still passes (monsters act, the status counts down).
        endTurn (addLog "You are paralysed and cannot move!" game)

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

            Search ->
                endTurn (searchTraps game)

            Fire ->
                tryFire game

            ThrowAt pos ->
                tryThrowAt pos game

            Brew ->
                tryBrew game

            Drop index ->
                dropItem index game

            Restart ->
                -- The shell (Main) owns reseeding a new run; inside a game it's a no-op.
                game

            NoOp ->
                game


{-| Does this message represent a turn-consuming hero action (the kind paralysis blocks)? -}
isActionMsg : Msg -> Bool
isActionMsg msg =
    case msg of
        Restart ->
            False

        NoOp ->
            False

        Drop _ ->
            False

        _ ->
            True


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
            if Level.at target game.level == LockedDoor then
                tryUnlock target game

            else if Level.at target game.level == Chasm then
                fallThroughChasm game

            else if Level.isPassableAt target game.level then
                let
                    hero =
                        game.hero

                    moved =
                        { hero | pos = target }

                    steppedTile =
                        Level.at target game.level

                    -- Stepping into a closed door opens it; trampling tall grass flattens it — both
                    -- stop the cell from blocking sight once you've passed through.
                    opened =
                        case steppedTile of
                            Door ->
                                Level.set target OpenDoor game.level

                            Grass ->
                                Level.set target Floor game.level

                            _ ->
                                game.level
                in
                endTurn (applyTerrainStep steppedTile (triggerTrap (tryBuy (pickUp (refreshFov { game | hero = moved, level = opened })))))

            else
                game


{-| Terrain underfoot when the hero steps: tall grass sometimes yields healing dew (foraging), and
water douses any fire the hero is carrying. -}
applyTerrainStep : Tile -> Game -> Game
applyTerrainStep tile game =
    let
        hero =
            game.hero
    in
    case tile of
        Grass ->
            let
                ( forage, seed1 ) =
                    Rng.chance 25 game.seed
            in
            if forage && hero.hp < hero.maxHp then
                { game | hero = { hero | hp = min hero.maxHp (hero.hp + 2) }, seed = seed1 }
                    |> addLog "You gather dew from the grass. (+2 HP)"

            else
                { game | seed = seed1 }

        Water ->
            if hasStatus Burn hero then
                { game | hero = { hero | statuses = List.filter (\s -> s.kind /= Burn) hero.statuses } }
                    |> addLog "You wade into the water; the flames hiss out."

            else
                game

        _ ->
            game


{-| Bump a locked door: if the hero is carrying a key, spend it and open the door (no movement this
turn); otherwise just report it's locked. -}
tryUnlock : Pos -> Game -> Game
tryUnlock door game =
    let
        hero =
            game.hero

        ( keys, rest ) =
            List.partition (\it -> isKey it) hero.inventory
    in
    case keys of
        _ :: remainingKeys ->
            endTurn
                (refreshFov
                    { game
                        | level = Level.set door Door game.level
                        , hero = { hero | inventory = remainingKeys ++ rest }
                    }
                    |> addLog "You unlock the door with an iron key."
                )

        [] ->
            addLog "The door is locked. You need a key." game


isKey : ItemDef -> Bool
isKey def =
    def.kind == Content.Key


tryDescend : Game -> Game
tryDescend game =
    if Level.at game.hero.pos game.level == StairsDown then
        let
            ( nextSeedA, nextSeedB ) =
                Rng.split game.seed

            gen =
                Dungeon.generate (Dungeon.configForDepth (game.depth + 1)) nextSeedA
        in
        enterLevel game.ruleset
            (game.depth + 1)
            game.kills
            game.idents
            nextSeedB
            gen
            game.hero
            (("You descend to depth " ++ String.fromInt (game.depth + 1) ++ ".") :: game.log)

    else
        addLog "There are no stairs down here." game


{-| Stepping into a chasm drops the hero to the next floor, taking falling damage on landing. -}
fallThroughChasm : Game -> Game
fallThroughChasm game =
    let
        ( nextSeedA, nextSeedB ) =
            Rng.split game.seed

        gen =
            Dungeon.generate (Dungeon.configForDepth (game.depth + 1)) nextSeedA

        fallen =
            enterLevel game.ruleset
                (game.depth + 1)
                game.kills
                game.idents
                nextSeedB
                gen
                game.hero
                ("You plunge through the chasm!" :: game.log)

        ( dmg, seed1 ) =
            Rng.range (game.depth + 1) (game.depth + 4) fallen.seed

        hero =
            fallen.hero
    in
    checkHeroDeath
        ({ fallen | hero = { hero | hp = hero.hp - dmg }, seed = seed1 }
            |> addLog ("You hit the ground hard (" ++ String.fromInt dmg ++ " damage).")
        )



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
    if it.def.id == "amulet" then
        { game | won = True, gameOver = True }
            |> addLog "You claim the Amulet of Yendor! Victory is yours!"

    else
        pickUpItem it game


pickUpItem : ItemOnFloor -> Game -> Game
pickUpItem it game =
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
                |> addLog ("You pick up a " ++ displayName game.idents it.def ++ ".")


{-| Alchemy: brew two potions from the pack into one fresh, depth-appropriate potion (a sink for
surplus potions, with a chance of something better). Needs at least two potions. -}
tryBrew : Game -> Game
tryBrew game =
    let
        hero =
            game.hero

        ( potions, others ) =
            List.partition isPotion hero.inventory
    in
    case potions of
        p1 :: p2 :: rest ->
            let
                candidates =
                    Content.itemsForDepth game.depth game.ruleset
                        |> List.filter (\( _, def ) -> isPotion def)

                ( brewed, seed1 ) =
                    Rng.pickWeighted p1 candidates game.seed
            in
            { game
                | hero = { hero | inventory = others ++ (brewed :: rest) }
                , seed = seed1
            }
                |> identify brewed
                |> addLog ("You brew " ++ withArticle (displayName game.idents brewed) ++ " from two potions.")
                |> endTurn

        _ ->
            addLog "You need two potions to brew something." game


{-| Drop the inventory item at `index` onto the hero's cell. Instant (no turn spent). -}
dropItem : Int -> Game -> Game
dropItem index game =
    case nth index game.hero.inventory of
        Nothing ->
            game

        Just def ->
            let
                hero =
                    game.hero
            in
            { game
                | hero = { hero | inventory = removeAt index hero.inventory }
                , items = { def = def, pos = hero.pos } :: game.items
            }
                |> addLog ("You drop the " ++ displayName game.idents def ++ ".")


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
                            identify def (applyEffect def game)

                        hero =
                            applied.hero
                    in
                    endTurn { applied | hero = { hero | inventory = removeAt index hero.inventory } }

                Content.Equipment slot _ ->
                    endTurn (equip index slot def game)

                Content.Wand spec ->
                    zapWand index spec game

                Content.Key ->
                    addLog "Keys open locked doors — walk into one." game


{-| Zap the wand in inventory slot `index` at the nearest visible monster, spending a charge. A
drained wand (0 charges) just fizzles until it recharges. No turn is spent on a fizzle or no target. -}
zapWand : Int -> Content.WandSpec -> Game -> Game
zapWand index spec game =
    if spec.charges <= 0 then
        addLog "The wand is drained — wait for it to recharge." game

    else
        case nearestVisibleEnemy game of
            Nothing ->
                addLog "You wave the wand, but there is no target in sight." game

            Just target ->
                let
                    ( dmg, seed1 ) =
                        rollDamage spec.damage target.def.defense game.seed

                    afterHit =
                        if target.hp - dmg <= 0 then
                            { game | enemies = List.filter (\e -> e.pos /= target.pos) game.enemies, seed = seed1, kills = game.kills + 1 }
                                |> addLog ("Your bolt destroys the " ++ target.def.name ++ "!")
                                |> addPopup target.pos (String.fromInt dmg) "#82aaff"
                                |> gainXp target.def.xp

                        else
                            { game
                                | enemies =
                                    updateEnemyAt target.pos
                                        (\e ->
                                            { e
                                                | hp = e.hp - dmg
                                                , alerted = True
                                                , statuses =
                                                    if spec.burns then
                                                        addEnemyStatus Burn 2 3 e.statuses

                                                    else
                                                        e.statuses
                                            }
                                        )
                                        game.enemies
                                , seed = seed1
                            }
                                |> addLog ("Your bolt hits the " ++ target.def.name ++ " (" ++ String.fromInt dmg ++ ").")
                                |> addPopup target.pos (String.fromInt dmg) "#82aaff"

                    hero =
                        afterHit.hero

                    drained =
                        List.indexedMap
                            (\i it ->
                                if i == index then
                                    { it | kind = Content.Wand { spec | charges = spec.charges - 1 } }

                                else
                                    it
                            )
                            hero.inventory
                in
                endTurn { afterHit | hero = { hero | inventory = drained } }


{-| A thrown attack at the monster on a chosen, visible cell (used by manual targeting). -}
tryThrowAt : Pos -> Game -> Game
tryThrowAt pos game =
    case enemyAt pos game of
        Just target ->
            if Set.member ( pos.x, pos.y ) game.visible then
                throwAtEnemy target game

            else
                addLog "You can't see your target." game

        Nothing ->
            addLog "Nothing to hit there." game


{-| A thrown attack at the nearest visible monster for half the hero's melee power (a sling/dagger
toss). Free to use but weak; costs a turn. No visible target → no turn spent. -}
tryFire : Game -> Game
tryFire game =
    case nearestVisibleEnemy game of
        Nothing ->
            addLog "You ready a throw, but see no target." game

        Just target ->
            throwAtEnemy target game


{-| Resolve a thrown attack against a specific enemy: half the hero's melee power, killing or hurting,
costing a turn. -}
throwAtEnemy : Enemy -> Game -> Game
throwAtEnemy target game =
    let
                power =
                    max 1 (heroDamage game.hero // 2)

                ( dmg, seed1 ) =
                    rollDamage power target.def.defense game.seed
            in
            endTurn
                (if target.hp - dmg <= 0 then
                    { game | enemies = List.filter (\e -> e.pos /= target.pos) game.enemies, seed = seed1, kills = game.kills + 1 }
                        |> addLog ("Your throw fells the " ++ target.def.name ++ "!")
                        |> addPopup target.pos (String.fromInt dmg) "#9be0ff"
                        |> gainXp target.def.xp

                 else
                    { game | enemies = updateEnemyAt target.pos (\e -> { e | hp = e.hp - dmg, alerted = True }) game.enemies, seed = seed1 }
                        |> addLog ("You throw at the " ++ target.def.name ++ " (" ++ String.fromInt dmg ++ ").")
                        |> addPopup target.pos (String.fromInt dmg) "#9be0ff"
                )


{-| The nearest monster the hero can currently see (within the visible set), if any. -}
nearestVisibleEnemy : Game -> Maybe Enemy
nearestVisibleEnemy game =
    game.enemies
        |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
        |> List.foldl
            (\e best ->
                case best of
                    Nothing ->
                        Just e

                    Just b ->
                        if Grid.chebyshev e.pos game.hero.pos < Grid.chebyshev b.pos game.hero.pos then
                            Just e

                        else
                            best
            )
            Nothing


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

                Content.RingSlot ->
                    hero.ring

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

                Content.RingSlot ->
                    { hero | inventory = pack, ring = Just def }
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

        name =
            displayName game.idents def
    in
    case effectOf def of
        HealHp n ->
            { game | hero = { hero | hp = min hero.maxHp (hero.hp + n) } }
                |> addLog ("You drink the " ++ name ++ ". (+" ++ String.fromInt n ++ " HP)")

        HealFull ->
            { game | hero = { hero | hp = hero.maxHp } }
                |> addLog ("You drink the " ++ name ++ ". You feel restored.")

        MaxHpBonus n ->
            { game | hero = { hero | maxHp = hero.maxHp + n, hp = hero.hp + n } }
                |> addLog ("You drink the " ++ name ++ ". (+" ++ String.fromInt n ++ " max HP)")

        DamageBonus n ->
            { game | hero = { hero | damage = hero.damage + n } }
                |> addLog ("You drink the " ++ name ++ ". You feel stronger.")

        DefenseBonus n ->
            { game | hero = { hero | defense = hero.defense + n } }
                |> addLog ("You drink the " ++ name ++ ". Your skin hardens.")

        Gold amount ->
            { game | hero = { hero | gold = hero.gold + amount } }
                |> addLog ("You gain " ++ String.fromInt amount ++ " gold.")

        Regenerate perTurn turns ->
            addStatus Regen perTurn turns game
                |> addLog ("You drink the " ++ name ++ ". You begin to mend.")

        TeleportSelf ->
            teleportHero game |> addLog "You read the scroll and blink away."

        MagicMap ->
            { game | explored = Set.fromList (List.map (\p -> ( p.x, p.y )) (Level.positions game.level)) }
                |> addLog "You read the scroll. The floor plan floods into your mind."

        IdentifyAll ->
            let
                idents =
                    game.idents

                allPotions =
                    game.ruleset.items |> List.filter isPotion |> List.map .id
            in
            { game | idents = { idents | known = Set.union idents.known (Set.fromList allPotions) } }
                |> addLog "You read the scroll. All potions are now familiar."

        UpgradeGear ->
            upgradeGear game

        Feed n ->
            { game | hero = { hero | nutrition = min maxNutrition (hero.nutrition + n) } }
                |> addLog ("You eat the " ++ name ++ ". You feel sated.")

        LightFor radius turns ->
            addStatus Lit radius turns game
                |> refreshFov
                |> addLog ("You light the " ++ name ++ ". The dark recedes.")

        HasteFor turns ->
            addStatus Hasted 1 turns game
                |> addLog ("You drink the " ++ name ++ ". You move with quickened speed!")


effectOf : ItemDef -> ItemEffect
effectOf def =
    case def.kind of
        Content.Consumable eff ->
            eff

        _ ->
            HealHp 0


{-| A scroll of upgrade enchants the worn weapon (or, lacking one, the worn armour): +1 to its
enchantment level, which `equipBonus` folds into the relevant stat. -}
upgradeGear : Game -> Game
upgradeGear game =
    let
        hero =
            game.hero
    in
    case hero.weapon of
        Just w ->
            { game | hero = { hero | weapon = Just (enchant w) } }
                |> addLog ("Your " ++ w.name ++ " glows — it is upgraded!")

        Nothing ->
            case hero.armour of
                Just a ->
                    { game | hero = { hero | armour = Just (enchant a) } }
                        |> addLog ("Your " ++ a.name ++ " glows — it is upgraded!")

                Nothing ->
                    addLog "You read the scroll, but have nothing equipped to upgrade." game


{-| Return a copy of an equipment item with its enchantment level raised by one. -}
enchant : ItemDef -> ItemDef
enchant item =
    case item.kind of
        Content.Equipment slot bonus ->
            { item | kind = Content.Equipment slot { bonus | plus = bonus.plus + 1 } }

        _ ->
            item



-- IDENTIFICATION ---------------------------------------------------------------------------------


{-| The pool of random looks an unidentified potion can wear this run. -}
palette : List Appearance
palette =
    [ { adjective = "murky", color = "#7f8b6a" }
    , { adjective = "azure", color = "#4f8bff" }
    , { adjective = "crimson", color = "#e0564b" }
    , { adjective = "fizzy", color = "#5dd47a" }
    , { adjective = "golden", color = "#d8b24c" }
    , { adjective = "violet", color = "#9b6ad8" }
    , { adjective = "smoky", color = "#9aa7ba" }
    , { adjective = "amber", color = "#e0824b" }
    ]


{-| Is this item a potion (and so subject to identification)? -}
isPotion : ItemDef -> Bool
isPotion def =
    case def.kind of
        Content.Consumable _ ->
            String.startsWith "potion" def.id

        _ ->
            False


{-| Assign each potion id a distinct random appearance for the run. -}
assignLooks : Ruleset -> Seed -> ( Dict String Appearance, Seed )
assignLooks ruleset seed =
    let
        potionIds =
            ruleset.items |> List.filter isPotion |> List.map .id

        ( shuffled, seed1 ) =
            Rng.shuffle palette seed
    in
    ( Dict.fromList (List.map2 Tuple.pair potionIds shuffled), seed1 )


{-| The name to show for an item: its true name once identified (or if not a potion), else its random
"<adjective> potion" appearance. -}
displayName : Idents -> ItemDef -> String
displayName idents def =
    case def.kind of
        Content.Wand spec ->
            def.name ++ " (" ++ String.fromInt spec.charges ++ ")"

        Content.Equipment _ bonus ->
            if bonus.plus > 0 then
                def.name ++ " +" ++ String.fromInt bonus.plus

            else
                def.name

        _ ->
            if not (isPotion def) || Set.member def.id idents.known then
                def.name

            else
                case Dict.get def.id idents.looks of
                    Just look ->
                        look.adjective ++ " potion"

                    Nothing ->
                        "potion"


{-| The colour to draw an item with: true colour once identified, else its appearance colour. -}
displayColor : Idents -> ItemDef -> String
displayColor idents def =
    if not (isPotion def) || Set.member def.id idents.known then
        def.color

    else
        case Dict.get def.id idents.looks of
            Just look ->
                look.color

            Nothing ->
                def.color


{-| Mark a potion identified (after drinking), announcing what it was if newly learned. -}
identify : ItemDef -> Game -> Game
identify def game =
    if isPotion def && not (Set.member def.id game.idents.known) then
        let
            idents =
                game.idents
        in
        { game | idents = { idents | known = Set.insert def.id idents.known } }
            |> addLog ("It was a " ++ def.name ++ "!")

    else
        game



-- TRAPS ------------------------------------------------------------------------------------------


{-| If the hero is standing on a trap, spring it: remove it and apply its effect. Triggered whether or
not it was revealed (you can deliberately walk onto a known trap, but usually you don't mean to). -}
triggerTrap : Game -> Game
triggerTrap game =
    case listFind (\t -> t.pos == game.hero.pos) game.traps of
        Nothing ->
            game

        Just trap ->
            { game | traps = List.filter (\t -> t.pos /= trap.pos) game.traps }
                |> trapEffect trap.kind


trapEffect : TrapKind -> Game -> Game
trapEffect kind game =
    case kind of
        DartTrap ->
            let
                ( dmg, s1 ) =
                    Rng.range (game.depth + 1) (game.depth + 3) game.seed
            in
            checkHeroDeath (damageHero dmg { game | seed = s1 } |> addLog ("A dart shoots out! (" ++ String.fromInt dmg ++ ")"))

        PoisonTrap ->
            addStatus Poison 2 5 game
                |> addLog "Poison gas hisses out! You are poisoned."

        TeleportTrap ->
            teleportHero game |> addLog "A teleport trap! You are flung across the floor."

        ParalysisTrap ->
            addStatus Paralyzed 1 4 game |> addLog "A paralysis trap! Your limbs lock up."


damageHero : Int -> Game -> Game
damageHero dmg game =
    let
        hero =
            game.hero
    in
    { game | hero = { hero | hp = hero.hp - dmg } }


{-| Relocate the hero to a random passable cell (used by teleport traps) and refresh fog. -}
teleportHero : Game -> Game
teleportHero game =
    let
        spots =
            List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing) (Level.positions game.level)

        ( dest, s1 ) =
            Rng.pick game.hero.pos spots game.seed

        hero =
            game.hero
    in
    refreshFov { game | hero = { hero | pos = dest }, seed = s1 }


{-| Reveal every hidden trap and secret door in the hero's eight neighbouring cells. -}
searchTraps : Game -> Game
searchTraps game =
    let
        near p =
            Grid.chebyshev p game.hero.pos <= 1

        revealedTraps =
            List.map
                (\t ->
                    if near t.pos then
                        { t | revealed = True }

                    else
                        t
                )
                game.traps

        trapsFound =
            List.length (List.filter (\t -> near t.pos && not t.revealed) game.traps)

        ( level1, doorsFound ) =
            revealSecretsNear (\p -> near p) game.level game.hero.pos

        total =
            trapsFound + doorsFound
    in
    { game | traps = revealedTraps, level = level1 }
        |> addLog
            (if total > 0 then
                "You find " ++ String.fromInt total ++ " hidden thing(s) nearby!"

             else
                "You search but find nothing."
            )


{-| Turn neighbouring `SecretDoor`s satisfying `pred` into ordinary `Door`s; returns the updated level
and how many were revealed. -}
revealSecretsNear : (Pos -> Bool) -> Level -> Pos -> ( Level, Int )
revealSecretsNear pred level origin =
    List.foldl
        (\nb ( lv, n ) ->
            if pred nb && Level.at nb lv == SecretDoor then
                ( Level.set nb Door lv, n + 1 )

            else
                ( lv, n )
        )
        ( level, 0 )
        (origin :: Grid.neighbors8 origin)



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
            |> addPopup enemy.pos (String.fromInt dmg) "#ffd166"
            |> gainXp enemy.def.xp

    else
        { game
            | enemies = updateEnemyAt enemy.pos (\e -> { e | hp = remaining, alerted = True }) game.enemies
            , seed = seed1
        }
            |> addLog ("You hit the " ++ enemy.def.name ++ " (" ++ String.fromInt dmg ++ ").")
            |> addPopup enemy.pos (String.fromInt dmg) "#ffd166"
            |> maybeSplit enemy remaining


{-| A `Splits` monster cleaves in two when wounded (but not killed): a fresh copy at half the parent's
remaining HP appears on a free adjacent cell, if there is one. -}
maybeSplit : Enemy -> Int -> Game -> Game
maybeSplit parent parentHp game =
    if parent.def.ability /= Content.Splits || parentHp <= 1 then
        game

    else
        let
            occupied =
                Set.fromList (( game.hero.pos.x, game.hero.pos.y ) :: List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies)

            free =
                Grid.neighbors8 parent.pos
                    |> List.filter (\p -> Level.isPassableAt p game.level && not (Set.member ( p.x, p.y ) occupied))
        in
        case free of
            spot :: _ ->
                let
                    childHp =
                        max 1 (parentHp // 2)

                    child =
                        { def = parent.def, pos = spot, hp = childHp, alerted = True, fleeing = False, statuses = [] }
                in
                { game
                    | enemies =
                        updateEnemyAt parent.pos (\e -> { e | hp = childHp }) game.enemies ++ [ child ]
                }
                    |> addLog ("The " ++ parent.def.name ++ " splits in two!")

            [] ->
                game



-- EXPERIENCE -------------------------------------------------------------------------------------


{-| Total XP needed to advance *out of* the given level. A gentle ramp: 10×level. -}
xpToNext : Int -> Int
xpToNext level =
    10 * level


{-| Award XP and apply any level-ups it unlocks (each grants HP/damage and a full heal). -}
gainXp : Int -> Game -> Game
gainXp amount game =
    let
        hero =
            game.hero
    in
    levelUps { game | hero = { hero | xp = hero.xp + amount } }


levelUps : Game -> Game
levelUps game =
    let
        hero =
            game.hero
    in
    if hero.xp >= xpToNext hero.level then
        let
            leveled =
                { hero
                    | xp = hero.xp - xpToNext hero.level
                    , level = hero.level + 1
                    , maxHp = hero.maxHp + 5
                    , damage = hero.damage + 1
                    , hp = hero.maxHp + 5
                }
        in
        levelUps ({ game | hero = leveled } |> addLog ("You reach level " ++ String.fromInt leveled.level ++ "! You feel mightier."))

    else
        game



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


{-| One monster's turn. It wakes on line of sight within `aggroRange` and stays alert thereafter
(remembering the hero). An alert monster: flees when badly hurt, melees an adjacent hero, shoots a hero
in range/sight if it has a ranged attack, or BFS-paths toward the hero (rounding corners). -}
stepEnemy : Enemy -> ( List Enemy, TurnAcc ) -> ( List Enemy, TurnAcc )
stepEnemy enemy ( done, acc ) =
    let
        heroPos =
            acc.hero.pos

        healed =
            applyRegen enemy

        dist =
            Grid.chebyshev healed.pos heroPos

        los =
            Fov.visibleFrom healed.pos heroPos acc.level

        aware =
            healed.alerted || (dist <= aggroRange && los)

        woken =
            { healed | alerted = aware }
    in
    if not aware then
        ( woken :: done, acc )

    else if dist == 1 then
        case woken.def.ability of
            Content.StealsGold amount ->
                stealAndFlee woken amount done acc

            _ ->
                attackHero woken (enemy.def.name ++ " hits you") done acc

    else if isFleeing woken then
        moveEnemy enemy woken (stepAway enemy.pos heroPos acc.level acc.occupied) done acc

    else if woken.def.ranged > 0 && dist <= woken.def.ranged && los then
        attackHero woken (enemy.def.name ++ " shoots you") done acc

    else
        moveEnemy enemy woken (Path.firstStep acc.level acc.occupied enemy.pos heroPos) done acc


{-| A `Regenerates` monster heals a little at the start of its turn. -}
applyRegen : Enemy -> Enemy
applyRegen enemy =
    case enemy.def.ability of
        Content.Regenerates n ->
            { enemy | hp = min enemy.def.maxHp (enemy.hp + n) }

        _ ->
            enemy


{-| A thief grabs gold and bolts for the exit. -}
stealAndFlee : Enemy -> Int -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
stealAndFlee enemy amount done acc =
    let
        hero =
            acc.hero

        stolen =
            min amount hero.gold
    in
    ( { enemy | fleeing = True } :: done
    , { acc
        | hero = { hero | gold = hero.gold - stolen }
        , log = ("The " ++ enemy.def.name ++ " steals " ++ String.fromInt stolen ++ " gold and flees!") :: acc.log
      }
    )


{-| A monster flees when badly hurt, or when it has stolen and wants to escape. -}
isFleeing : Enemy -> Bool
isFleeing enemy =
    enemy.fleeing || enemy.hp * 4 < enemy.def.maxHp


attackHero : Enemy -> String -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
attackHero enemy verb done acc =
    let
        ( dmg, seed1 ) =
            rollDamage enemy.def.damage (heroDefense acc.hero) acc.seed

        hero =
            acc.hero
    in
    ( enemy :: done
    , { acc
        | hero = { hero | hp = hero.hp - dmg }
        , seed = seed1
        , log = ("The " ++ verb ++ " (" ++ String.fromInt dmg ++ ").") :: acc.log
      }
    )


moveEnemy : Enemy -> Enemy -> Maybe Pos -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
moveEnemy original woken maybeNext done acc =
    case maybeNext of
        Just next ->
            ( { woken | pos = next } :: done
            , { acc
                | occupied =
                    acc.occupied
                        |> Set.remove ( original.pos.x, original.pos.y )
                        |> Set.insert ( next.x, next.y )
              }
            )

        Nothing ->
            ( woken :: done, acc )


{-| Step to the passable, unoccupied neighbour that most *increases* distance from the target. -}
stepAway : Pos -> Pos -> Level -> Set ( Int, Int ) -> Maybe Pos
stepAway from to level occupied =
    Grid.eightDirs
        |> List.map (Grid.move from)
        |> List.filter (\p -> Level.isPassableAt p level && not (Set.member ( p.x, p.y ) occupied))
        |> maximumBy (\p -> Grid.chebyshev p to)


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
            Fov.compute (fovRadiusFor game.hero game.depth) game.hero.pos game.level
    in
    { game | visible = vis, explored = Set.union game.explored vis }


{-| Close out a turn-consuming hero action: run the monsters, then tick the counter. -}
endTurn : Game -> Game
endTurn game =
    let
        afterStatuses =
            tickStatuses game

        tempo =
            afterStatuses.tempo + 1

        bumped =
            { afterStatuses | tempo = tempo }

        -- Haste lets the hero act on every tick while monsters act on every other; slow does the
        -- reverse (monsters get a bonus turn). Neither → the usual one-for-one.
        enemyPhases =
            if hasStatus Hasted bumped.hero then
                modBy 2 tempo

            else if hasStatus Slowed bumped.hero then
                2

            else
                1

        afterEnemyDot =
            tickEnemyStatuses bumped

        afterMonsters =
            applyTimes enemyPhases enemiesTurn afterEnemyDot

        afterPerception =
            passivePerception afterMonsters

        afterHunger =
            tickHunger afterPerception

        recharged =
            rechargeWands { afterHunger | turn = afterHunger.turn + 1 }
    in
    maybeWander recharged


{-| Every 12 turns each wand in the pack regains one charge, up to its maximum. -}
rechargeWands : Game -> Game
rechargeWands game =
    if modBy 12 game.turn /= 0 then
        game

    else
        let
            hero =
                game.hero

            bumped =
                List.map
                    (\it ->
                        case it.kind of
                            Content.Wand spec ->
                                if spec.charges < spec.maxCharges then
                                    { it | kind = Content.Wand { spec | charges = spec.charges + 1 } }

                                else
                                    it

                            _ ->
                                it
                    )
                    hero.inventory
        in
        { game | hero = { hero | inventory = bumped } }


{-| Every so often a fresh monster wanders onto the floor — out of the hero's sight — so camping isn't
free. Capped a little above the floor's normal population. -}
maybeWander : Game -> Game
maybeWander game =
    if game.gameOver || modBy 40 game.turn /= 0 || List.length game.enemies >= Content.spawnCountForDepth game.depth + 3 then
        game

    else
        let
            candidates =
                Content.enemiesForDepth game.depth game.ruleset

            occupied =
                Set.insert ( game.hero.pos.x, game.hero.pos.y ) (Set.fromList (List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies))

            spots =
                Level.positions game.level
                    |> List.filter
                        (\p ->
                            Level.at p game.level
                                == Floor
                                && not (Set.member ( p.x, p.y ) game.visible)
                                && not (Set.member ( p.x, p.y ) occupied)
                        )
        in
        case ( candidates, spots ) of
            ( ( _, firstDef ) :: _, _ ) ->
                let
                    ( spot, s1 ) =
                        Rng.pick game.hero.pos spots game.seed

                    ( def, s2 ) =
                        Rng.pickWeighted firstDef candidates s1
                in
                if List.isEmpty spots then
                    game

                else
                    { game
                        | enemies = { def = def, pos = spot, hp = def.maxHp, alerted = False, fleeing = False, statuses = [] } :: game.enemies
                        , seed = s2
                    }

            _ ->
                game


{-| Add or refresh a status on a monster (mirrors the hero's `addStatus`). -}
addEnemyStatus : StatusKind -> Int -> Int -> List Status -> List Status
addEnemyStatus kind magnitude turns statuses =
    { kind = kind, magnitude = magnitude, turns = turns } :: List.filter (\s -> s.kind /= kind) statuses


{-| Tick every monster's damage-over-time (burn/poison): apply the damage, count down the statuses,
remove any monster it kills (awarding XP and a popup). -}
tickEnemyStatuses : Game -> Game
tickEnemyStatuses game =
    let
        step e ( alive, killXp, killN, pops, logs ) =
            let
                dot =
                    e.statuses
                        |> List.filter (\s -> s.kind == Burn || s.kind == Poison)
                        |> List.map .magnitude
                        |> List.sum

                ticked =
                    e.statuses |> List.map (\s -> { s | turns = s.turns - 1 }) |> List.filter (\s -> s.turns > 0)

                newHp =
                    e.hp - dot
            in
            if dot > 0 && newHp <= 0 then
                ( alive
                , killXp + e.def.xp
                , killN + 1
                , { pos = e.pos, text = String.fromInt dot, color = "#ff7a3c" } :: pops
                , ("The " ++ e.def.name ++ " succumbs.") :: logs
                )

            else if dot > 0 then
                ( { e | hp = newHp, statuses = ticked } :: alive
                , killXp
                , killN
                , { pos = e.pos, text = String.fromInt dot, color = "#ff7a3c" } :: pops
                , logs
                )

            else
                ( { e | statuses = ticked } :: alive, killXp, killN, pops, logs )

        ( survivors, xp, n, popups, logs ) =
            List.foldl step ( [], 0, 0, [], [] ) game.enemies
    in
    { game
        | enemies = List.reverse survivors
        , kills = game.kills + n
        , popups = popups ++ game.popups
        , log = logs ++ game.log
    }
        |> gainXp xp


applyTimes : Int -> (a -> a) -> a -> a
applyTimes n f x =
    if n <= 0 then
        x

    else
        applyTimes (n - 1) f (f x)


{-| Burn one point of nutrition per turn; at zero the hero starves for 1 HP a turn. -}
tickHunger : Game -> Game
tickHunger game =
    let
        hero =
            game.hero

        fed =
            hero.nutrition - 1
    in
    if fed <= 0 then
        checkHeroDeath ({ game | hero = { hero | hp = hero.hp - 1, nutrition = 0 } } |> addLog "You are starving!")

    else
        { game | hero = { hero | nutrition = fed } }


{-| Each turn there's a chance the hero spots an adjacent secret door without searching, so one never
permanently blocks the way. -}
passivePerception : Game -> Game
passivePerception game =
    let
        ( notice, seed1 ) =
            Rng.chance 25 game.seed
    in
    if notice then
        let
            ( level1, found ) =
                revealSecretsNear (\p -> Grid.chebyshev p game.hero.pos == 1) game.level game.hero.pos
        in
        if found > 0 then
            { game | level = level1, seed = seed1 } |> addLog "You notice a hidden door!"

        else
            { game | seed = seed1 }

    else
        { game | seed = seed1 }


{-| Apply each active status to the hero (poison/burn drain HP, regen restores it, capped at max),
count it down, and drop the expired ones. Runs once per turn-consuming action. -}
tickStatuses : Game -> Game
tickStatuses game =
    let
        hero =
            game.hero

        ( hpDelta, logs ) =
            List.foldl
                (\status ( dhp, ls ) ->
                    case status.kind of
                        Regen ->
                            ( dhp + status.magnitude, ls )

                        Poison ->
                            ( dhp - status.magnitude, ("Poison gnaws at you (" ++ String.fromInt status.magnitude ++ ").") :: ls )

                        Burn ->
                            ( dhp - status.magnitude, ("Flames sear you (" ++ String.fromInt status.magnitude ++ ").") :: ls )

                        _ ->
                            ( dhp, ls )
                )
                ( 0, [] )
                hero.statuses

        ticked =
            hero.statuses
                |> List.map (\s -> { s | turns = s.turns - 1 })
                |> List.filter (\s -> s.turns > 0)

        newHp =
            min hero.maxHp (hero.hp + hpDelta)
    in
    checkHeroDeath
        { game
            | hero = { hero | hp = newHp, statuses = ticked }
            , log = logs ++ game.log
        }


{-| Add a status, or refresh the duration of one already active of the same kind. -}
addStatus : StatusKind -> Int -> Int -> Game -> Game
addStatus kind magnitude turns game =
    let
        hero =
            game.hero

        others =
            List.filter (\s -> s.kind /= kind) hero.statuses
    in
    { game | hero = { hero | statuses = { kind = kind, magnitude = magnitude, turns = turns } :: others } }


nth : Int -> List a -> Maybe a
nth i xs =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i xs)


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f xs =
    case xs of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl (\x best -> if f x > f best then x else best) first rest)


addLog : String -> Game -> Game
addLog line game =
    { game | log = line :: game.log }


addPopup : Pos -> String -> String -> Game -> Game
addPopup pos text color game =
    { game | popups = { pos = pos, text = text, color = color } :: game.popups }


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
        List.map trapGlyph (List.filter .revealed game.traps)
            ++ List.map shopGlyph game.shop
            ++ List.map (itemGlyph game.idents) game.items
            ++ List.map enemyGlyph game.enemies
            ++ [ heroGlyph game ]
    , popups = List.map (\p -> { pos = p.pos, text = p.text, color = p.color }) game.popups
    , theme = Render.themeForDepth game.depth
    , camera = game.hero.pos
    , cursor = Nothing
    , hud =
        { title = "elm-rogue"
        , region = (Render.themeForDepth game.depth).name
        , level = game.hero.level
        , xp = game.hero.xp
        , xpNext = xpToNext game.hero.level
        , depth = game.depth
        , hp = game.hero.hp
        , maxHp = game.hero.maxHp
        , turn = game.turn
        , gold = game.hero.gold
        , hunger = hungerLabel game.hero.nutrition
        , weapon = equippedName game.hero.weapon (heroDamage game.hero) "dmg"
        , armour = equippedName game.hero.armour (heroDefense game.hero) "def"
        , ring =
            case game.hero.ring of
                Just r ->
                    r.name

                Nothing ->
                    ""
        , statuses = List.map statusLabel game.hero.statuses
        , inventory = List.map (displayName game.idents) game.hero.inventory
        , log = List.take 7 game.log
        , gameOver = game.gameOver
        , won = game.won
        , status = statusLine game
        }
    }


hungerLabel : Int -> String
hungerLabel nutrition =
    if nutrition > 300 then
        ""

    else if nutrition > 100 then
        "Hungry"

    else if nutrition > 0 then
        "Famished"

    else
        "Starving"


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


{-| A HUD label for an equipped slot: the item's name (with enchant level, or a dash) plus the
resulting total stat. -}
equippedName : Maybe ItemDef -> Int -> String -> String
equippedName maybeItem total label =
    let
        prefix =
            case maybeItem of
                Just item ->
                    case item.kind of
                        Content.Equipment _ bonus ->
                            if bonus.plus > 0 then
                                item.name ++ " +" ++ String.fromInt bonus.plus

                            else
                                item.name

                        _ ->
                            item.name

                Nothing ->
                    "—"
    in
    prefix ++ " (" ++ label ++ " " ++ String.fromInt total ++ ")"


itemGlyph : Idents -> ItemOnFloor -> Render.Glyph
itemGlyph idents item =
    { pos = item.pos
    , char = item.def.glyph
    , color = displayColor idents item.def
    , layer = Render.layerItem
    , heavy = False
    }


shopGlyph : ShopEntry -> Render.Glyph
shopGlyph entry =
    { pos = entry.pos
    , char = entry.def.glyph
    , color = "#ffd166"
    , layer = Render.layerItem
    , heavy = False
    }


trapGlyph : Trap -> Render.Glyph
trapGlyph trap =
    { pos = trap.pos
    , char = "^"
    , color = "#e0824b"
    , layer = Render.layerTerrain
    , heavy = False
    }
