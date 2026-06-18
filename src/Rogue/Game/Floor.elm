module Rogue.Game.Floor exposing (..)

{-| Building a floor: enterLevel (place the hero, spawn the monster population and features), the
per-feature builders, and the bump-interactions for wells/plants/shops. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
import Rogue.Dungeon as Dungeon exposing (Generated, Room)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Path as Path
import Rogue.Rng as Rng exposing (Seed)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)
import Rogue.Game.Types exposing (..)
import Rogue.Game.Appearance exposing (..)
import Rogue.Game.Combat exposing (..)
import Rogue.Game.Sim exposing (..)
import Rogue.Game.MonsterTurn exposing (..)
import Rogue.Game.ItemEffects exposing (..)


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

        -- Density scales with floor size but is capped, so big floors stay challenging, not swarming.
        enemyCount =
            min (floorCount // 6) (Content.spawnCountForDepth depth + floorCount // 55)

        ( enemies, seed2 ) =
            spawnEnemies ruleset depth (List.take enemyCount shuffledSpots) seed1 gen.level

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
            spawnFeatures ruleset depth (floorFeatures gen) seed5 gen.level

        bossEnemy =
            case Content.bossForDepth depth ruleset of
                Just def ->
                    [ { def = def, pos = bossSpot gen, hp = def.maxHp, alerted = False, fleeing = False, statuses = [], ally = False, revives = 0 } ]

                Nothing ->
                    []

        -- A "feeling" on arrival, SPD-style: a quick read of how this floor is shaping up.
        feeling =
            if not (List.isEmpty bossEnemy) then
                "A powerful presence guards this floor!"

            else if List.length enemies >= Content.spawnCountForDepth depth + 4 then
                "This floor crawls with danger."

            else if List.length enemies <= 1 then
                "An uneasy quiet hangs in the air."

            else if List.length items >= Content.itemCountForDepth depth + 2 then
                "You sense the glint of treasure nearby."

            else
                ""

        bossLog =
            if feeling == "" then
                log

            else
                feeling :: log

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

        -- A locked chest with its own key, plus an occasional altar — from the leftover floor cells.
        leftover =
            List.drop (enemyCount + itemCount + trapCount + 30) shuffledSpots

        ( chests, chestKey, seed8 ) =
            buildChest ruleset depth gen leftover seed7

        ( altar, seed9 ) =
            buildAltar (List.drop 3 leftover) seed8

        ( npc, seed10 ) =
            buildNpc ruleset depth (List.drop 6 leftover) seed9

        ( plants, seed11 ) =
            buildPlants (List.drop 9 leftover) seed10

        ( well, seed12 ) =
            buildWell (List.drop 15 leftover) seed11

        ( statues, seed13 ) =
            buildStatues depth (List.drop 21 leftover) seed12

        vis =
            withTorchlight gen.level (Fov.compute (fovRadiusFor heroAt depth) heroAt.pos gen.level)
    in
    { ruleset = ruleset
    , level = gen.level
    , rooms = gen.rooms
    , hero = heroAt
    , enemies = enemies ++ featureEnemies ++ bossEnemy
    , items = items ++ vaultItems ++ featureItems ++ amuletItems ++ chestKey
    , shop = shop
    , chests = chests
    , altar = altar
    , well = well
    , statues = statues
    , npc = npc
    , quest = Nothing
    , challenges = []
    , remains = Nothing
    , ascending = False
    , awaitingGhostGift = False
    , awaitingEnchant = False
    , popups = []
    , traps = traps
    , gas = Dict.empty
    , fire =
        -- The Demon Halls smoulder: a few short-lived embers dot the deepest floors.
        if depth >= 11 then
            List.take 3 (List.drop 18 leftover)
                |> List.map (\p -> ( ( p.x, p.y ), 2 ))
                |> Dict.fromList

        else
            Dict.empty
    , ice = Dict.empty
    , plants = plants
    , idents = idents
    , depth = depth
    , turn = 0
    , tempo = 0
    , kills = kills
    , seed = seed13
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
spawnEnemies : Ruleset -> Int -> List Pos -> Seed -> Level -> ( List Enemy, Seed )
spawnEnemies ruleset depth spots seed level =
    let
        candidates =
            Content.enemiesForDepth depth ruleset
    in
    case candidates of
        [] ->
            ( [], seed )

        ( _, firstDef ) :: _ ->
            let
                ( enemies, _, finalSeed ) =
                    List.foldl
                        (\pos ( acc, occupied, s ) ->
                            if Set.member ( pos.x, pos.y ) occupied then
                                ( acc, occupied, s )

                            else
                                let
                                    ( def, s2 ) =
                                        Rng.pickWeighted firstDef candidates s

                                    ( edef, s3 ) =
                                        maybeChampion depth def s2

                                    leader =
                                        { def = edef, pos = pos, hp = edef.maxHp, alerted = False, fleeing = False, statuses = [], ally = False, revives = initialRevives edef }

                                    -- Social monsters arrive with kin: pack-mates fill nearby free cells.
                                    packSpots =
                                        if packSize def.id > 0 then
                                            Grid.neighbors8 pos
                                                |> List.filter (\p -> level7Passable p level && not (Set.member ( p.x, p.y ) occupied) && p /= pos)
                                                |> List.take (packSize def.id)

                                        else
                                            []

                                    packMates =
                                        List.map (\p -> { def = def, pos = p, hp = def.maxHp, alerted = False, fleeing = False, statuses = [], ally = False, revives = initialRevives def }) packSpots

                                    occupied2 =
                                        List.foldl (\p o -> Set.insert ( p.x, p.y ) o) (Set.insert ( pos.x, pos.y ) occupied) packSpots
                                in
                                ( leader :: packMates ++ acc, occupied2, s3 )
                        )
                        ( [], Set.empty, seed )
                        spots
            in
            ( enemies, finalSeed )


{-| How many times a monster can claw back from death on a melee kill (ghouls rise once). -}
initialRevives : EnemyDef -> Int
initialRevives def =
    if def.id == "ghoul" then
        1

    else
        0


{-| How many extra pack-mates a monster type brings (0 = solitary). Social early-floor monsters travel
in packs, deepening the "ambushed by a group" feel without changing the floor's spawn-point count. -}
packSize : String -> Int
packSize id =
    case id of
        "rat" ->
            2

        "marsupial-rat" ->
            2

        "gnoll-scout" ->
            1

        "swarm" ->
            1

        "cave-bat" ->
            1

        _ ->
            0


{-| Passability check for pack placement (a plain floor-ish cell, not a wall/door). -}
level7Passable : Pos -> Level -> Bool
level7Passable p level =
    Level.isPassableAt p level


{-| Now and then (more often deeper) a monster is promoted to a **champion** with one of four
modifiers, each a distinct buff, tint and name: a **giant** (huge HP, heavy hits), a **blazing** one
(sets you alight on contact), a **projecting** one (gains a ranged bolt) or a **blessed** one (much
tougher to wound). Champions are worth triple XP. -}
maybeChampion : Int -> EnemyDef -> Seed -> ( EnemyDef, Seed )
maybeChampion depth def seed =
    let
        ( roll, seed1 ) =
            Rng.int 100 seed
    in
    if roll < min 14 (4 + depth) then
        let
            ( pick, seed2 ) =
                Rng.int 4 seed1

            champ =
                case pick of
                    0 ->
                        { def | name = "giant " ++ def.name, maxHp = def.maxHp * 2 + 6, damage = def.damage + 3, color = "#e0884b" }

                    1 ->
                        { def | name = "blazing " ++ def.name, maxHp = (def.maxHp * 3) // 2, ability = Content.Burns, color = "#ff7a3c" }

                    2 ->
                        { def | name = "projecting " ++ def.name, maxHp = (def.maxHp * 3) // 2, ranged = max 4 def.ranged, color = "#9be0ff" }

                    _ ->
                        { def | name = "blessed " ++ def.name, maxHp = (def.maxHp * 3) // 2, defense = def.defense + 3, color = "#ffe08a" }
        in
        ( { champ | xp = def.xp * 3 }, seed2 )

    else
        ( def, seed1 )


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
            [ ( 5, DartTrap ), ( 4, PoisonTrap ), ( 3, AlarmTrap ) ]
                ++ (if depth >= 3 then
                        [ ( 3, TeleportTrap ), ( 3, ParalysisTrap ), ( 3, FrostTrap ) ]

                    else
                        []
                   )
                ++ (if depth >= 5 then
                        [ ( 3, RockfallTrap ), ( 3, FlameTrap ), ( 3, SummonTrap ) ]

                    else
                        []
                   )
                ++ (if depth >= 4 then
                        [ ( 2, PitfallTrap ) ]

                    else
                        []
                   )
    in
    Rng.pickWeighted DartTrap candidates seed


{-| Place one locked chest (with valuable loot) and drop its key elsewhere on the floor, so the loot
is always reachable. Returns the chest, the key item to scatter, and the advanced seed. -}
buildChest : Ruleset -> Int -> Generated -> List Pos -> Seed -> ( List Chest, List ItemOnFloor, Seed )
buildChest ruleset depth gen spots seed =
    let
        loot =
            Content.itemsForDepth depth ruleset
                |> List.filter (\( _, def ) -> isGearLoot def)
    in
    case ( spots, loot, Content.findItem "key" ruleset ) of
        ( chestPos :: keyPos :: _, ( _, firstDef ) :: _, Just keyDef ) ->
            let
                ( lootDef, seed1 ) =
                    Rng.pickWeighted firstDef loot seed

                ( isCrystal, seed2 ) =
                    Rng.chance 30 seed1

                -- Crystal chests are transparent: you see the loot inside and they never hide a mimic.
                ( isMimic, seed3 ) =
                    if isCrystal then
                        ( False, seed2 )

                    else
                        Rng.chance 25 seed2
            in
            ( [ { pos = chestPos, loot = lootDef, mimic = isMimic, crystal = isCrystal } ], [ { def = keyDef, pos = keyPos } ], seed3 )

        _ ->
            ( [], [], seed )


isGearLoot : ItemDef -> Bool
isGearLoot def =
    case def.kind of
        Content.Equipment _ _ ->
            True

        Content.Wand _ ->
            True

        Content.Artifact _ ->
            True

        _ ->
            False


{-| A third of floors host a friendly NPC at a free cell — a ghost, a sage, a wandmaker, a blacksmith
or a bounty-offering imp, chosen at random. -}
buildNpc : Ruleset -> Int -> List Pos -> Seed -> ( Maybe Npc, Seed )
buildNpc ruleset depth spots seed =
    let
        ( present, seed1 ) =
            Rng.chance 33 seed

        ( pick, seed2 ) =
            Rng.int 5 seed1

        kind =
            case pick of
                0 ->
                    Sage

                1 ->
                    Wandmaker

                2 ->
                    Blacksmith

                3 ->
                    Imp

                _ ->
                    Ghost

        -- Wandmakers/imps reward a wand; others, any gift.
        pool =
            Content.itemsForDepth depth ruleset
                |> List.filter
                    (\( _, def ) ->
                        if kind == Wandmaker || kind == Imp then
                            isWand def

                        else
                            def.id /= "amulet" && def.id /= "gold" && def.id /= "key"
                    )
    in
    case ( present, spots, pool ) of
        ( True, p :: _, ( _, firstDef ) :: _ ) ->
            let
                ( reward, seed3 ) =
                    Rng.pickWeighted firstDef pool seed2
            in
            ( Just { pos = p, kind = kind, reward = reward }, seed3 )

        _ ->
            ( Nothing, seed2 )


{-| Scatter a handful of plants on free cells; each is sprung (and consumed) by whoever steps on it. -}
buildPlants : List Pos -> Seed -> ( Dict ( Int, Int ) PlantKind, Seed )
buildPlants spots seed =
    List.foldl
        (\p ( acc, s ) ->
            let
                ( roll, s2 ) =
                    Rng.int 4 s

                kind =
                    case roll of
                        0 ->
                            Firebloom

                        1 ->
                            Sungrass

                        2 ->
                            Sorrowmoss

                        _ ->
                            Earthroot
            in
            ( Dict.insert ( p.x, p.y ) kind acc, s2 )
        )
        ( Dict.empty, seed )
        (List.take 5 spots)


{-| Spring the plant (if any) the hero just stepped on: trigger its effect and remove it. -}
stepOnPlant : Game -> Game
stepOnPlant game =
    case Dict.get ( game.hero.pos.x, game.hero.pos.y ) game.plants of
        Nothing ->
            game

        Just kind ->
            let
                cleared =
                    { game | plants = Dict.remove ( game.hero.pos.x, game.hero.pos.y ) game.plants }

                hero =
                    cleared.hero
            in
            case kind of
                Firebloom ->
                    spawnFire game.hero.pos cleared |> addLog "You crush a firebloom — it erupts in flame!"

                Sungrass ->
                    { cleared | hero = { hero | hp = min hero.maxHp (hero.hp + 8) } }
                        |> addLog "You brush against sungrass — its dew mends your wounds. (+8 HP)"

                Sorrowmoss ->
                    addStatus Poison 2 5 cleared |> addLog "You tread on sorrowmoss — its spores sicken you."

                Earthroot ->
                    addStatus Paralyzed 1 3 cleared |> addLog "Earthroot snares your legs!"


{-| About a third of floors hold a magic well at a free cell. -}
buildWell : List Pos -> Seed -> ( Maybe Well, Seed )
buildWell spots seed =
    let
        ( present, seed1 ) =
            Rng.chance 35 seed

        ( pick, seed2 ) =
            Rng.int 3 seed1

        kind =
            case pick of
                0 ->
                    AwarenessWell

                1 ->
                    TransmuteWell

                _ ->
                    HealthWell
    in
    case ( present, spots ) of
        ( True, p :: _ ) ->
            ( Just { pos = p, kind = kind }, seed2 )

        _ ->
            ( Nothing, seed2 )


{-| Place a couple of dormant guardian statues on deeper floors (none in the shallow Sewers). -}
buildStatues : Int -> List Pos -> Seed -> ( List Pos, Seed )
buildStatues depth spots seed =
    if depth < 3 then
        ( [], seed )

    else
        let
            ( n, seed1 ) =
                Rng.int 3 seed
        in
        ( List.take n spots, seed1 )


{-| A statue grinds to life into a guardian when the hero comes within two cells. -}
{-| Step onto a magic well to drink its one-time draught. -}
stepOnWell : Game -> Game
stepOnWell game =
    case game.well of
        Just w ->
            if w.pos == game.hero.pos then
                let
                    cleared =
                        { game | well = Nothing }

                    hero =
                        cleared.hero
                in
                case w.kind of
                    HealthWell ->
                        { cleared | hero = { hero | maxHp = hero.maxHp + 3, hp = hero.maxHp + 3 } }
                            |> addLog "You drink from the Well of Health — vitality surges through you."

                    AwarenessWell ->
                        applyEffect { id = "_well", name = "well", glyph = "", color = "", kind = Content.Consumable MagicMap, minDepth = 0, maxDepth = 0, spawnWeight = 0 } cleared
                            |> addLog "The Well of Awareness reveals the floor to your mind."

                    TransmuteWell ->
                        transmuteItem cleared
                            |> addLog "The Well of Transmutation reshapes one of your possessions."

            else
                game

        Nothing ->
            game


{-| Half the floors host an altar at a free cell: stepping onto it once fully heals the hero. -}
buildAltar : List Pos -> Seed -> ( Maybe Pos, Seed )
buildAltar spots seed =
    let
        ( hasAltar, seed1 ) =
            Rng.chance 50 seed
    in
    case ( hasAltar, spots ) of
        ( True, p :: _ ) ->
            ( Just p, seed1 )

        _ ->
            ( Nothing, seed1 )


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


spawnFeatures : Ruleset -> Int -> List Dungeon.Feature -> Seed -> Level -> ( List Enemy, List ItemOnFloor, Seed )
spawnFeatures ruleset depth features seed level =
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
                            spawnEnemies ruleset depth (List.take 4 feature.cells) s level
                    in
                    ( enemies ++ accE, accI, s2 )

                Dungeon.Pit ->
                    ( accE, accI, s )

                Dungeon.Garden ->
                    let
                        seeds =
                            ruleset.items
                                |> List.filter (\i -> String.startsWith "seed-" i.id)

                        ( items, s2 ) =
                            spawnFrom seeds depth (List.take 2 feature.cells) s
                    in
                    ( accE, items ++ accI, s2 )

                Dungeon.Armory ->
                    let
                        gear =
                            ruleset.items
                                |> List.filter isGearLoot

                        ( items, s2 ) =
                            spawnFrom gear depth (List.take 3 feature.cells) s
                    in
                    ( accE, items ++ accI, s2 )
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

                        ( cursedDef, s3 ) =
                            maybeCurse def s2
                    in
                    ( { def = cursedDef, pos = pos } :: acc, s3 )
                )
                ( [], seed )
                spots


{-| About one in five pieces of dropped equipment is cursed: a negative enchantment that sticks until
a scroll of remove curse (or upgrade) cleanses it. -}
maybeCurse : ItemDef -> Seed -> ( ItemDef, Seed )
maybeCurse def seed =
    case def.kind of
        Content.Equipment slot bonus ->
            let
                ( curse, seed1 ) =
                    Rng.chance 20 seed
            in
            if curse then
                ( { def | kind = Content.Equipment slot { bonus | cursed = True, plus = bonus.plus - 2 } }, seed1 )

            else
                ( def, seed1 )

        _ ->
            ( def, seed )


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




-- spawnRemainsIfHere (moved from Rogue.Game) -----------------------------------------------------

spawnRemainsIfHere : Game -> Game
spawnRemainsIfHere game =
    case game.remains of
        Just r ->
            if r.depth == game.depth then
                let
                    hero =
                        game.hero

                    item =
                        Content.findItem r.itemId game.ruleset
                            |> Maybe.map (\def -> [ { def = def, pos = game.stairsUp } ])
                            |> Maybe.withDefault []
                in
                { game
                    | remains = Nothing
                    , hero = { hero | gold = hero.gold + r.gold }
                    , items = item ++ game.items
                }
                    |> addLog ("You find the remains of a fallen adventurer and recover " ++ String.fromInt r.gold ++ " gold.")

            else
                game

        Nothing ->
            game

