module Rogue.Game exposing
    ( Game
    , Hero
    , Enemy
    , Msg(..)
    , SaveData
    , newGame
    , resume
    , knownItemIds
    , chooseSubclass
    , learnTalent
    , subclassChoices
    , talentChoices
    , startChallenges
    , challengeChoices
    , alchemyRecipes
    , itemCatalog
    , nearShop
    , takeGhostGift
    , chooseEnchant
    , setRemains
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
import Rogue.Game.Types exposing (..)
import Rogue.Game.Appearance exposing (..)
import Rogue.Game.Combat exposing (..)
import Rogue.Game.Sim exposing (..)
import Rogue.Game.MonsterTurn exposing (..)
import Rogue.Game.Scene exposing (toScene)


type Msg
    = Move Dir
    | Descend
    | Wait
    | Use Int
    | Search
    | Fire
    | ThrowAt Pos
    | Brew
    | Ability
    | Examine
    | AutoExplore
    | Drop Int
    | Sell Int
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
            , subclass = Nothing
            , talents = []
            , heroClass = class.id
            , abilityCharge = 0
            }
    in
    let
        ( looks, seedA ) =
            assignLooks ruleset gen.seed

        idents =
            { known = Set.empty, looks = looks }
    in
    enterLevel ruleset 1 0 idents seedA gen hero [ "You enter the dungeon as " ++ withArticle class.name ++ "." ]




{-| The ids of every item type the hero has identified — for persisting in a save. -}
knownItemIds : Game -> List String
knownItemIds game =
    Set.toList game.idents.known


{-| Continue a saved run: regenerate a floor at the saved depth and reinstate the hero (stats, gear
and inventory resolved from the ruleset). Within-floor progress isn't kept — you resume your character
at the start of a fresh floor at that depth. -}
resume : Ruleset -> SaveData -> Game
resume ruleset save =
    let
        gen =
            Dungeon.generate (Dungeon.configForDepth save.depth) (Rng.seed save.seed)

        find id =
            Content.findItem id ruleset

        hero =
            { pos = gen.stairsUp
            , hp = save.hp
            , maxHp = save.maxHp
            , damage = save.damage
            , defense = save.defense
            , inventory = List.filterMap find save.inventoryIds
            , gold = save.gold
            , weapon = Maybe.andThen find save.weaponId
            , armour = Maybe.andThen find save.armourId
            , ring = Maybe.andThen find save.ringId
            , glyph = save.glyph
            , color = save.color
            , fovRadius = save.fovRadius
            , statuses = []
            , level = save.level
            , xp = save.xp
            , nutrition = save.nutrition
            , subclass = Nothing
            , talents = []
            , heroClass = ""
            , abilityCharge = 0
            }

        ( looks, seedA ) =
            assignLooks ruleset gen.seed

        idents =
            { known = Set.fromList save.knownIds, looks = looks }
    in
    enterLevel ruleset save.depth 0 idents seedA gen hero [ "You resume your delve on depth " ++ String.fromInt save.depth ++ "." ]


{-| Commit to a subclass (chosen at the depth threshold). Some subclasses grant an immediate bonus
(the Sentinel's extra vitality); all confer a passive read by `heroDamage`/`heroDefense`/`heroAttack`. -}
chooseSubclass : String -> Game -> Game
chooseSubclass name game =
    let
        hero =
            game.hero

        bonusHp =
            if name == "Sentinel" then
                8

            else
                0
    in
    { game | hero = { hero | subclass = Just name, maxHp = hero.maxHp + bonusHp, hp = hero.hp + bonusHp } }
        |> addLog ("You embrace the path of the " ++ name ++ "!")


{-| Learn a talent (chosen on level-up). `Toughness` raises max HP at once; the rest are passives. -}
learnTalent : String -> Game -> Game
learnTalent name game =
    let
        hero =
            game.hero

        bonusHp =
            case name of
                "Toughness" ->
                    5

                "Vitality" ->
                    10

                _ ->
                    0

        fovBonus =
            if name == "Keen Eye" then
                1

            else
                0

        learned =
            { hero
                | talents = name :: hero.talents
                , maxHp = hero.maxHp + bonusHp
                , hp =
                    if name == "Second Wind" then
                        hero.maxHp + bonusHp

                    else
                        hero.hp + bonusHp
                , fovRadius = hero.fovRadius + fovBonus
            }
    in
    { game | hero = learned }
        |> addLog ("You master the " ++ name ++ " talent.")


{-| Record the remains of a previous death on a fresh run, and place them immediately if the starting
floor is the right depth. -}
setRemains : Remains -> Game -> Game
setRemains r game =
    spawnRemainsIfHere { game | remains = Just r }


{-| If the hero has reached the depth where their predecessor fell, materialise the remains: recover
the gold at once and drop the saved item on the start cell, then clear the remains. -}
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


{-| Apply chosen run-modifier challenges to a fresh game (records them; some, like "Frailty", also
adjust the starting hero). Ids are checked at the relevant engine hooks. -}
startChallenges : List String -> Game -> Game
startChallenges ids game =
    let
        hero =
            game.hero

        frail =
            if List.member "frailty" ids then
                { hero | maxHp = max 8 (hero.maxHp - 8), hp = max 8 (hero.hp - 8) }

            else
                hero

        stripped =
            if List.member "minimalist" ids then
                { frail | inventory = [] }

            else
                frail

        bareHanded =
            if List.member "bare-handed" ids then
                { stripped | weapon = Nothing, armour = Nothing }

            else
                stripped
    in
    { game | challenges = ids, hero = bareHanded }


{-| The optional run-modifier challenges (id, label, one-line effect). -}
challengeChoices : List ( String, ( String, String ) )
challengeChoices =
    [ ( "no-healing", ( "Pharmacophobia", "healing potions do nothing" ) )
    , ( "glass-cannon", ( "Glass Cannon", "you deal and take double damage" ) )
    , ( "darkness", ( "Into Darkness", "your sight is dimmed (−2 FOV)" ) )
    , ( "frailty", ( "Frailty", "start with 8 less max HP" ) )
    , ( "no-scrolls", ( "Forbidden Runes", "scrolls crumble unread" ) )
    , ( "starvation", ( "On a Diet", "hunger sets in twice as fast" ) )
    , ( "badder-bosses", ( "Badder Bosses", "bosses are enraged from the start" ) )
    , ( "minimalist", ( "Minimalist", "begin with an empty pack" ) )
    , ( "bare-handed", ( "Bare-Handed", "begin with no weapon or armour" ) )
    , ( "swarming", ( "Swarming", "monsters wander in far more often" ) )
    ]


{-| The subclasses offered at the depth threshold (label + one-line effect). -}
subclassChoices : List ( String, String )
subclassChoices =
    [ ( "Duellist", "+2 damage to every strike" )
    , ( "Berserker", "+4 damage while badly wounded" )
    , ( "Sentinel", "+8 max HP and +2 defense" )
    , ( "Stalker", "surprise strikes deal triple" )
    ]


{-| The tiered talent tree: each talent has a minimum hero level (T1 from L2, T2 from L4, T3 from L7). -}
talentChoices : List { name : String, desc : String, minLevel : Int }
talentChoices =
    [ { name = "Sharpened Edge", desc = "+1 damage", minLevel = 2 }
    , { name = "Iron Will", desc = "+1 defense", minLevel = 2 }
    , { name = "Toughness", desc = "+5 max HP", minLevel = 2 }
    , { name = "Deadly Strike", desc = "+2 damage", minLevel = 4 }
    , { name = "Keen Eye", desc = "+1 sight radius", minLevel = 4 }
    , { name = "Vitality", desc = "+10 max HP", minLevel = 4 }
    , { name = "Heroism", desc = "+3 damage", minLevel = 7 }
    , { name = "Bulwark", desc = "+3 defense", minLevel = 7 }
    , { name = "Second Wind", desc = "heal fully now", minLevel = 7 }
    , { name = "Executioner", desc = "+4 damage", minLevel = 7 }
    , { name = "Bloodlust", desc = "+3 damage while below half HP", minLevel = 7 }
    , { name = "Last Stand", desc = "+4 defense while below half HP", minLevel = 7 }
    ]


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

    else if hasStatus Charmed game.hero && isActionMsg msg && charmFalters game then
        -- Charmed: now and then you stand transfixed, losing the action while the world moves on.
        endTurn (addLog "You stand transfixed, charmed!" { game | seed = Tuple.second (Rng.int 2 game.seed) })

    else
        case msg of
            Move dir ->
                tryMove dir game

            Descend ->
                if game.ascending then
                    tryAscend game

                else
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

            Ability ->
                useAbility game

            Examine ->
                examine game

            AutoExplore ->
                autoExplore game

            Drop index ->
                dropItem index game

            Sell index ->
                sellItem index game

            Restart ->
                -- The shell (Main) owns reseeding a new run; inside a game it's a no-op.
                game

            NoOp ->
                game


{-| Does this message represent a turn-consuming hero action (the kind paralysis blocks)? -}
{-| Whether a charmed hero falters this action (≈50%), losing the turn to fascination. -}
charmFalters : Game -> Bool
charmFalters game =
    Tuple.first (Rng.chance 50 game.seed)


isActionMsg : Msg -> Bool
isActionMsg msg =
    case msg of
        Restart ->
            False

        NoOp ->
            False

        Examine ->
            False

        Drop _ ->
            False

        Sell _ ->
            False

        _ ->
            True


{-| Take one step toward the nearest unexplored floor cell (or the down-stairs once the floor is fully
mapped), pathing around walls. Stops with a warning if a monster is in view. -}
autoExplore : Game -> Game
autoExplore game =
    if List.any (\e -> not e.ally && Set.member ( e.pos.x, e.pos.y ) game.visible) game.enemies then
        addLog "There are monsters about — you explore on your own." game

    else
        let
            unexplored =
                Level.positions game.level
                    |> List.filter (\p -> Level.at p game.level == Floor && not (Set.member ( p.x, p.y ) game.explored))

            target =
                maximumBy (\p -> negate (Grid.chebyshev p game.hero.pos)) unexplored
                    |> Maybe.withDefault game.stairsDown

            occupied =
                Set.fromList (List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies)
        in
        if target == game.hero.pos then
            addLog "There is nothing left to explore here." game

        else
            case Path.firstStep game.level occupied game.hero.pos target of
                Just step ->
                    tryMove { x = step.x - game.hero.pos.x, y = step.y - game.hero.pos.y } game

                Nothing ->
                    addLog "You can't find a route to explore further." game


{-| Look around: report the nearest visible monster (its stats and any special trait), or the terrain
underfoot if none is in sight. Costs no turn. -}
examine : Game -> Game
examine game =
    case nearestVisibleEnemy game of
        Just e ->
            addLog
                ("You study the "
                    ++ e.def.name
                    ++ ": HP "
                    ++ String.fromInt (max 0 e.hp)
                    ++ "/"
                    ++ String.fromInt e.def.maxHp
                    ++ ", dmg "
                    ++ String.fromInt e.def.damage
                    ++ ", def "
                    ++ String.fromInt e.def.defense
                    ++ abilityNote e.def.ability
                    ++ "."
                )
                game

        Nothing ->
            addLog ("You see no foes. You stand on " ++ Tile.name (Level.at game.hero.pos game.level) ++ ".") game


abilityNote : Content.MonsterAbility -> String
abilityNote ability =
    case ability of
        Content.NoAbility ->
            ""

        Content.Regenerates _ ->
            " (regenerates)"

        Content.Splits ->
            " (splits)"

        Content.StealsGold _ ->
            " (steals gold)"

        Content.Burns ->
            " (ignites on hit)"

        Content.Heals _ ->
            " (heals allies)"

        Content.SummonsAllies ->
            " (summons)"

        Content.Aquatic ->
            " (aquatic)"

        Content.Weakens ->
            " (hexes on hit)"

        Content.Spores ->
            " (spews spores)"

        Content.Charms ->
            " (charms on hit)"


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
            endTurn (heavyRecovery (heroAttack enemy game))

        Nothing ->
            -- Reach weapons (spear) strike a foe two cells away in a straight line, through the empty
            -- cell you'd have stepped into.
            case reachTarget dir game of
                Just farEnemy ->
                    endTurn (heroAttack farEnemy game)

                Nothing ->
                    moveOrInteract dir target game


{-| The enemy a reach weapon can hit: two cells out in `dir`, when the hero wields a reach weapon and
the intervening cell is passable and empty. -}
reachTarget : Dir -> Game -> Maybe Enemy
reachTarget dir game =
    if not (weaponReach game.hero.weapon) then
        Nothing

    else
        let
            mid =
                Grid.move game.hero.pos dir

            far =
                Grid.move mid dir
        in
        if Level.isPassableAt mid game.level && enemyAt mid game == Nothing then
            enemyAt far game |> Maybe.andThen (\e -> if e.ally then Nothing else Just e)

        else
            Nothing


{-| Does the wielded weapon have extended reach (hits at range 2)? -}
weaponReach : Maybe ItemDef -> Bool
weaponReach maybeWeapon =
    case maybeWeapon of
        Just w ->
            w.id == "spear" || w.id == "glaive" || w.id == "grim-glaive"

        Nothing ->
            False


{-| Heavy weapons hit hard but are unwieldy: each swing leaves the hero briefly Slowed, so foes get an
extra beat to act. The big base damage lives in the weapon's stats; this is the recovery cost. -}
heavyRecovery : Game -> Game
heavyRecovery game =
    let
        heavy =
            case game.hero.weapon of
                Just w ->
                    w.id == "warhammer" || w.id == "blazing-mace"

                Nothing ->
                    False
    in
    if heavy then
        addStatus Slowed 1 1 game

    else
        game


moveOrInteract : Dir -> Pos -> Game -> Game
moveOrInteract dir target game =
            if npcAt target game then
                talkToNpc game

            else if chestAt target game /= Nothing then
                tryOpenChest target game

            else if Level.at target game.level == LockedDoor then
                tryUnlock target game

            else if Level.at target game.level == Chasm && not (hasStatus Levitating game.hero) then
                fallThroughChasm game

            else if Level.isPassableAt target game.level || (Level.at target game.level == Chasm && hasStatus Levitating game.hero) then
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
                endTurn (stepOnWell (stepOnPlant (blessAtAltar (applyTerrainStep steppedTile (triggerTrap (tryBuy (pickUp (refreshFov { game | hero = moved, level = opened }))))))))

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


npcAt : Pos -> Game -> Bool
npcAt p game =
    game.npc |> Maybe.map (\n -> n.pos == p) |> Maybe.withDefault False


{-| Talk to the NPC on the bumped cell: a ghost gifts its reward, a sage identifies all potions. The
NPC then departs. Doesn't move the hero. -}
talkToNpc : Game -> Game
talkToNpc game =
    case game.npc of
        Nothing ->
            game

        Just n ->
            let
                hero =
                    game.hero
            in
            case n.kind of
                Ghost ->
                    -- The sad ghost offers a *choice* of parting gift; the UI resolves it via
                    -- `takeGhostGift`. We stash the offered reward in npc-less state by keeping the
                    -- flag; the rose option is fixed, the blade option is the ghost's own reward.
                    endTurn
                        ({ game | npc = Nothing, awaitingGhostGift = True, quest = Just { targetKills = 0, targetDepth = 0, reward = n.reward, giver = "ghost" } }
                            |> addLog "The sad ghost pleads: 'Free me, and take a token — the rose, or my old blade.'"
                        )

                Sage ->
                    let
                        idents =
                            game.idents

                        allPotions =
                            game.ruleset.items |> List.filter isPotion |> List.map .id
                    in
                    endTurn
                        ({ game | npc = Nothing, idents = { idents | known = Set.union idents.known (Set.fromList allPotions) } }
                            |> addLog "The sage murmurs over your pack; all potions are now known. They depart."
                        )

                Wandmaker ->
                    endTurn
                        ({ game
                            | npc = Nothing
                            , quest = Just { targetKills = 0, targetDepth = game.depth + 2, reward = n.reward, giver = "wandmaker" }
                         }
                            |> addLog ("The wandmaker asks you to fetch a reagent two floors deeper; their reward will find you there.")
                        )

                Blacksmith ->
                    case ( hero.weapon, Content.findItem "scroll-upgrade" game.ruleset ) of
                        ( Just _, Just upgradeScroll ) ->
                            -- A smith's bargain: mine him dark ore from deeper down (reach depth+2),
                            -- and he forwards a reforging scroll for your trouble.
                            endTurn
                                ({ game
                                    | npc = Nothing
                                    , quest = Just { targetKills = 0, targetDepth = game.depth + 2, reward = upgradeScroll, giver = "blacksmith" }
                                 }
                                    |> addLog "The blacksmith strikes a bargain: bring him dark ore from two floors down for a reforging."
                                )

                        ( Just _, Nothing ) ->
                            endTurn
                                ({ game | npc = Nothing, hero = { hero | weapon = Maybe.map enchant hero.weapon } }
                                    |> addLog "The blacksmith reforges your weapon — it gleams sharper (+1)."
                                )

                        ( Nothing, _ ) ->
                            endTurn
                                ({ game | npc = Nothing }
                                    |> addLog "The blacksmith shrugs — you carry no weapon to reforge."
                                )

                Imp ->
                    endTurn
                        ({ game
                            | npc = Nothing
                            , quest = Just { targetKills = game.kills + 6, targetDepth = 0, reward = n.reward, giver = "imp" }
                         }
                            |> addLog "The ambitious imp offers a bounty: slay 6 more monsters and a reward is yours."
                        )


{-| Resolve the sad ghost's parting gift once the player picks. "rose" grants the Dried Rose artifact;
anything else grants the ghost's stashed reward weapon. Clears the pending flag and the ghost quest. -}
takeGhostGift : String -> Game -> Game
takeGhostGift choice game =
    if not game.awaitingGhostGift then
        game

    else
        let
            hero =
                game.hero

            reward =
                if choice == "rose" then
                    Content.findItem "dried-rose" game.ruleset

                else
                    Maybe.map .reward game.quest

            cleared =
                { game | awaitingGhostGift = False, quest = Nothing }
        in
        case reward of
            Just item ->
                { cleared | hero = { hero | inventory = hero.inventory ++ [ item ] } }
                    |> addLog ("The ghost sighs in relief as you take the " ++ displayName game.idents item ++ ", and fades away.")

            Nothing ->
                cleared |> addLog "The ghost fades away."


{-| Apply the player's chosen weapon enchantment (from the scroll-of-enchantment choice modal). -}
chooseEnchant : String -> Game -> Game
chooseEnchant ench game =
    if not game.awaitingEnchant then
        game

    else
        let
            hero =
                game.hero
        in
        { game | awaitingEnchant = False, hero = { hero | weapon = Maybe.map (setEnchant ench) hero.weapon } }
            |> addLog ("Runes blaze across your weapon — it is now " ++ ench ++ ".")


chestAt : Pos -> Game -> Maybe Chest
chestAt p game =
    listFind (\c -> c.pos == p) game.chests


{-| Bump a chest: a key opens it (loot to the pack); without one it stays shut (no turn spent). -}
tryOpenChest : Pos -> Game -> Game
tryOpenChest pos game =
    case chestAt pos game of
        Nothing ->
            game

        Just chest ->
            let
                hero =
                    game.hero

                ( keys, rest ) =
                    List.partition isKey hero.inventory
            in
            case keys of
                _ :: remainingKeys ->
                    if chest.mimic then
                        endTurn (springMimic chest { game | hero = { hero | inventory = remainingKeys ++ rest } })

                    else
                        endTurn
                            ({ game
                                | chests = List.filter (\c -> c.pos /= pos) game.chests
                                , hero = { hero | inventory = (remainingKeys ++ rest) ++ [ chest.loot ] }
                             }
                                |> addLog ("You unlock the chest and find a " ++ displayName game.idents chest.loot ++ "!")
                            )

                [] ->
                    if chest.crystal then
                        addLog ("Through the crystal chest you glimpse a " ++ displayName game.idents chest.loot ++ " — you need a key.") game

                    else
                        addLog "The chest is locked. You need a key." game


{-| A mimic chest lurches into a monster (a tough disguised predator) when opened. -}
springMimic : Chest -> Game -> Game
springMimic chest game =
    let
        candidates =
            Content.enemiesForDepth game.depth game.ruleset

        ( base, seed1 ) =
            case candidates of
                ( _, firstDef ) :: _ ->
                    Rng.pickWeighted firstDef candidates game.seed

                [] ->
                    ( placeholderEnemyDef, game.seed )

        mimicDef =
            { base
                | name = "mimic"
                , glyph = "m"
                , color = "#caa24a"
                , maxHp = base.maxHp * 2 + 10
                , damage = base.damage + 3
                , ability = Content.NoAbility
                , xp = base.xp * 2
            }

        spot =
            Grid.neighbors8 chest.pos
                |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing && p /= game.hero.pos)
                |> List.head
                |> Maybe.withDefault chest.pos

        mimic =
            { def = mimicDef, pos = spot, hp = mimicDef.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
    in
    { game
        | chests = List.filter (\c -> c.pos /= chest.pos) game.chests
        , enemies = mimic :: game.enemies
        , seed = seed1
    }
        |> addLog "The chest lunges — it's a mimic!"


{-| A spectral ally summoned by the dried rose: scales gently with depth. -}
ghostAllyDef : Int -> EnemyDef
ghostAllyDef depth =
    { id = "ghost-ally"
    , name = "spectral ally"
    , glyph = "G"
    , color = "#a9d6ff"
    , maxHp = 14 + depth * 2
    , damage = 5 + depth
    , defense = 2
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 0
    , drop = Nothing
    }


{-| A short-lived friendly bee, released from a honeypot, that harries nearby foes. -}
beeAllyDef : Int -> EnemyDef
beeAllyDef depth =
    { id = "bee-ally"
    , name = "swarm of bees"
    , glyph = "w"
    , color = "#ecd24c"
    , maxHp = 8 + depth
    , damage = 3 + depth // 2
    , defense = 1
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 0
    , drop = Nothing
    }


{-| A harmless sheep — from a polymorph scroll or a decoy. Deals no damage; as an ally it draws enemy
attacks (a living shield) and blocks corridors. -}
sheepDef : EnemyDef
sheepDef =
    { id = "sheep"
    , name = "sheep"
    , glyph = "s"
    , color = "#e6ebf4"
    , maxHp = 4
    , damage = 0
    , defense = 0
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 0
    , drop = Nothing
    }


placeholderEnemyDef : EnemyDef
placeholderEnemyDef =
    { id = "mimic"
    , name = "mimic"
    , glyph = "m"
    , color = "#caa24a"
    , maxHp = 20
    , damage = 6
    , defense = 2
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 8
    , drop = Nothing
    }


{-| Stepping onto an altar grants its one-time blessing: full health. -}
blessAtAltar : Game -> Game
blessAtAltar game =
    if game.altar == Just game.hero.pos then
        let
            hero =
                game.hero

            -- The altar's blessing also lifts curses from worn gear.
            uncurse maybeItem =
                Maybe.map uncurseItem maybeItem

            cleansed =
                { hero
                    | hp = hero.maxHp
                    , weapon = uncurse hero.weapon
                    , armour = uncurse hero.armour
                    , ring = uncurse hero.ring
                }

            hadCurse =
                List.any isCursed (List.filterMap identity [ hero.weapon, hero.armour, hero.ring ])

            ( boon, seed1 ) =
                Rng.int 3 game.seed

            baseLog =
                if hadCurse then
                    "You kneel at the altar; it heals you and lifts your curses"

                else
                    "You kneel at the altar; it heals you"

            ( blessedHero, boonLog ) =
                case boon of
                    0 ->
                        ( { cleansed | maxHp = cleansed.maxHp + 3, hp = cleansed.maxHp + 3 }, ", and toughens your flesh (+3 max HP)." )

                    1 ->
                        ( { cleansed | statuses = addEnemyStatus Shielded 10 25 cleansed.statuses }, ", and wraps you in a shield." )

                    _ ->
                        ( { cleansed | abilityCharge = abilityMax }, ", and charges your ability." )
        in
        { game | hero = blessedHero, altar = Nothing, seed = seed1 }
            |> addLog (baseLog ++ boonLog)

    else
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




tryDescend : Game -> Game
tryDescend game =
    if Level.at game.hero.pos game.level == StairsDown && List.any (\e -> e.def.boss) game.enemies then
        addLog "A magical seal binds the stairs shut — the floor's guardian must fall first." game

    else if Level.at game.hero.pos game.level == StairsDown then
        let
            ( nextSeedA, nextSeedB ) =
                Rng.split game.seed

            gen =
                Dungeon.generate (Dungeon.configForDepth (game.depth + 1)) nextSeedA

            descended =
                enterLevel game.ruleset
                    (game.depth + 1)
                    game.kills
                    game.idents
                    nextSeedB
                    gen
                    game.hero
                    (("You descend to depth " ++ String.fromInt (game.depth + 1) ++ ".") :: game.log)
        in
        spawnRemainsIfHere { descended | quest = game.quest, challenges = game.challenges, remains = game.remains }

    else
        addLog "There are no stairs down here." game


{-| Climb the up-stairs during the Amulet ascension. Reaching the surface (above depth 1) wins; each
floor climbed regenerates with its monsters roused and hunting. -}
tryAscend : Game -> Game
tryAscend game =
    if Level.at game.hero.pos game.level /= StairsUp then
        addLog "Carrying the Amulet, you must climb the up-stairs to escape." game

    else if game.depth <= 1 then
        { game | won = True, gameOver = True }
            |> addLog "You burst into daylight, Amulet in hand — you have escaped! Victory!"

    else
        let
            ( nextSeedA, nextSeedB ) =
                Rng.split game.seed

            gen =
                Dungeon.generate (Dungeon.configForDepth (game.depth - 1)) nextSeedA

            climbed =
                enterLevel game.ruleset
                    (game.depth - 1)
                    game.kills
                    game.idents
                    nextSeedB
                    gen
                    game.hero
                    (("You climb to depth " ++ String.fromInt (game.depth - 1) ++ ".") :: game.log)
        in
        -- Keep ascending; rouse the whole floor against the Amulet-bearer.
        { climbed
            | quest = game.quest
            , challenges = game.challenges
            , ascending = True
            , enemies = List.map (\e -> { e | alerted = True }) climbed.enemies
        }


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
        (spawnRemainsIfHere { fallen | hero = { hero | hp = hero.hp - dmg }, seed = seed1, quest = game.quest, challenges = game.challenges, remains = game.remains }
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
        -- Claiming the Amulet begins the **ascension**: every monster awakens and you must climb back
        -- to the surface (depth 1's up-stairs) to win.
        { game | ascending = True, enemies = List.map (\e -> { e | alerted = True }) game.enemies }
            |> addLog "You claim the Amulet of Yendor! Now escape to the surface — the dungeon awakens!"

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




{-| Unleash the hero's class armour ability if charged (then reset its charge). Each class has its own:
the Warrior slams adjacent foes, the Mage blasts everything in sight, the Rogue vanishes in smoke, the
Huntress looses spectral blades, the Duelist lunges at the nearest foe. -}
useAbility : Game -> Game
useAbility game =
    let
        hero =
            game.hero
    in
    if hero.abilityCharge < abilityMax then
        addLog ("Your ability is still charging (" ++ String.fromInt hero.abilityCharge ++ "/" ++ String.fromInt abilityMax ++ ").") game

    else
        let
            spent =
                { game | hero = { hero | abilityCharge = 0 } }
        in
        case hero.heroClass of
            "warrior" ->
                heroSlam spent |> endTurn

            "mage" ->
                psionicBlast (10 + game.depth * 2) spent
                    |> addLog "You unleash an elemental blast!"
                    |> endTurn

            "rogue" ->
                addStatus Invisible 1 12 (teleportHero spent)
                    |> addLog "You drop a smoke bomb and vanish!"
                    |> endTurn

            "huntress" ->
                psionicBlast (8 + game.depth * 2) spent
                    |> addLog "You loose a volley of spectral blades!"
                    |> endTurn

            "duelist" ->
                duelistLunge spent |> endTurn

            _ ->
                addLog "You have no special ability." game


{-| Warrior slam: heavy damage to every adjacent monster. -}
heroSlam : Game -> Game
heroSlam game =
    let
        power =
            heroDamage game.hero + 6

        adjacent e =
            Grid.chebyshev e.pos game.hero.pos == 1

        step e ( alive, xp, kills, pops ) =
            if adjacent e then
                let
                    hp =
                        e.hp - power
                in
                if hp <= 0 then
                    ( alive, xp + e.def.xp, kills + 1, { pos = e.pos, text = String.fromInt power, color = "#ffd166" } :: pops )

                else
                    ( { e | hp = hp, alerted = True } :: alive, xp, kills, { pos = e.pos, text = String.fromInt power, color = "#ffd166" } :: pops )

            else
                ( e :: alive, xp, kills, pops )

        ( survivors, gained, killed, popups ) =
            List.foldl step ( [], 0, 0, [] ) game.enemies
    in
    { game | enemies = List.reverse survivors, kills = game.kills + killed, popups = popups ++ game.popups }
        |> gainXp gained
        |> addLog "You slam the ground, crushing nearby foes!"


{-| Duelist lunge: a single devastating strike on the nearest visible monster. -}
duelistLunge : Game -> Game
duelistLunge game =
    case nearestVisibleEnemy game of
        Nothing ->
            addLog "You lunge, but find no mark." game

        Just target ->
            let
                power =
                    heroDamage game.hero * 3

                hp =
                    target.hp - power
            in
            if hp <= 0 then
                { game | enemies = List.filter (\e -> e.pos /= target.pos) game.enemies, kills = game.kills + 1 }
                    |> addLog ("You lunge and run the " ++ target.def.name ++ " through!")
                    |> addPopup target.pos (String.fromInt power) "#ff7adf"
                    |> gainXp target.def.xp
                    |> dropLoot target

            else
                { game | enemies = updateEnemyAt target.pos (\e -> { e | hp = hp, alerted = True }) game.enemies }
                    |> addLog ("You lunge at the " ++ target.def.name ++ " (" ++ String.fromInt power ++ ")!")
                    |> addPopup target.pos (String.fromInt power) "#ff7adf"


{-| Alchemy: brew two potions from the pack into one fresh, depth-appropriate potion (a sink for
surplus potions, with a chance of something better). Needs at least two potions. -}
{-| Known alchemy recipes: exact ingredient ids → a guaranteed output. Checked before the random brew. -}
alchemyRecipes : List { inputs : List String, output : String, name : String }
alchemyRecipes =
    [ { inputs = [ "potion-healing", "potion-healing" ], output = "potion-greater-healing", name = "Full Healing" }
    , { inputs = [ "potion-liquid-flame", "potion-caustic-gas" ], output = "bomb", name = "Bomb" }
    , { inputs = [ "potion-strength", "potion-healing" ], output = "potion-experience", name = "Experience" }
    , { inputs = [ "darts", "potion-liquid-flame" ], output = "javelin", name = "Javelin" }
    , { inputs = [ "darts", "torch" ], output = "fire-darts", name = "Incendiary Darts" }
    , { inputs = [ "darts", "potion-caustic-gas" ], output = "poison-darts", name = "Poison Darts" }
    , { inputs = [ "potion-haste", "potion-invisibility" ], output = "potion-levitation", name = "Levitation" }
    , { inputs = [ "ration", "mystery-meat" ], output = "cooked-meal", name = "Cooked Meal" }
    , { inputs = [ "ration", "ration" ], output = "hearty-feast", name = "Hearty Feast" }
    ]


{-| Alchemy: if the pack holds a known recipe's exact ingredients, brew its guaranteed output; otherwise
fall back to fusing any two potions into a random one. -}
tryBrew : Game -> Game
tryBrew game =
    case List.filter (\r -> hasIngredients r.inputs game.hero.inventory) alchemyRecipes of
        recipe :: _ ->
            case Content.findItem recipe.output game.ruleset of
                Just out ->
                    let
                        hero =
                            game.hero
                    in
                    { game | hero = { hero | inventory = removeIngredients recipe.inputs hero.inventory ++ [ out ] } }
                        |> identify out
                        |> addLog ("Following the recipe, you brew " ++ withArticle (displayName game.idents out) ++ "!")
                        |> endTurn

                Nothing ->
                    randomBrew game

        [] ->
            randomBrew game


{-| Does the inventory contain every ingredient id (with multiplicity)? -}
hasIngredients : List String -> List ItemDef -> Bool
hasIngredients inputs inventory =
    case inputs of
        [] ->
            True

        first :: rest ->
            case findIndex (\it -> it.id == first) inventory of
                Just idx ->
                    hasIngredients rest (removeAt idx inventory)

                Nothing ->
                    False


removeIngredients : List String -> List ItemDef -> List ItemDef
removeIngredients inputs inventory =
    case inputs of
        [] ->
            inventory

        first :: rest ->
            case findIndex (\it -> it.id == first) inventory of
                Just idx ->
                    removeIngredients rest (removeAt idx inventory)

                Nothing ->
                    removeIngredients rest inventory


randomBrew : Game -> Game
randomBrew game =
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
                |> addLog ("You fuse two potions into " ++ withArticle (displayName game.idents brewed) ++ ".")
                |> endTurn

        _ ->
            addLog "You need a known recipe's ingredients, or two potions, to brew." game


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


{-| Is the hero standing in (or right beside) the floor's shop, so it can trade? -}
nearShop : Game -> Bool
nearShop game =
    List.any (\e -> Grid.chebyshev e.pos game.hero.pos <= 4) game.shop


{-| The gold a shopkeeper pays for an item — half its asking price, floored at a token amount. -}
resaleValue : Game -> ItemDef -> Int
resaleValue game def =
    max 5 (priceFor game.depth def // 2)


{-| Sell the inventory item at `index` for gold — only near the floor's shop. Instant (no turn). -}
sellItem : Int -> Game -> Game
sellItem index game =
    case nth index game.hero.inventory of
        Nothing ->
            game

        Just def ->
            if not (nearShop game) then
                addLog "You can only sell at a shop." game

            else
                let
                    hero =
                        game.hero

                    value =
                        resaleValue game def
                in
                { game | hero = { hero | inventory = removeAt index hero.inventory, gold = hero.gold + value } }
                    |> addLog ("Sold the " ++ displayName game.idents def ++ " for " ++ String.fromInt value ++ " gold.")


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
                Content.Consumable eff ->
                    if def.id == "ankh" then
                        addLog "The ankh hums with protective magic — it will revive you when you fall." game

                    else if isScroll def && List.member "no-scrolls" game.challenges then
                        addLog ("The " ++ displayName game.idents def ++ " crumbles to dust — its runes are forbidden to you.") game

                    else if isThrown eff then
                        throwConsumable index def eff game

                    else
                        let
                            applied =
                                identify def (applyEffect def game)

                            hero =
                                applied.hero
                        in
                        endTurn { applied | hero = { hero | inventory = removeAt index hero.inventory } }

                Content.Equipment slot _ ->
                    endTurn (identify def (equip index slot def game))

                Content.Wand spec ->
                    identify def (zapWand index spec game)

                Content.Artifact spec ->
                    useArtifact index spec def game

                Content.Key ->
                    addLog "Keys open locked doors — walk into one." game


{-| Effects that are *thrown* rather than drunk: they shatter at a target cell (the nearest visible
monster, or the hero's feet) and unleash their hazard there. -}
isThrown : ItemEffect -> Bool
isThrown eff =
    case eff of
        Incinerate _ ->
            True

        ToxicGas _ ->
            True

        Explode _ ->
            True

        ThrownHit _ ->
            True

        ThrownTipped _ _ ->
            True

        Freeze _ ->
            True

        _ ->
            False


{-| Lob a throwable consumable at the nearest visible monster (or the hero's own cell if none), bursting
its effect there, then consume it. -}
throwConsumable : Int -> ItemDef -> ItemEffect -> Game -> Game
throwConsumable index def eff game =
    let
        target =
            nearestVisibleEnemy game |> Maybe.map .pos |> Maybe.withDefault game.hero.pos

        hero =
            game.hero

        consumed =
            { game | hero = { hero | inventory = removeAt index hero.inventory } }

        name =
            displayName game.idents def

        resolved =
            applyThrownEffect eff target { consumed | seed = consumed.seed }
                |> identify def
                |> addLog ("You hurl the " ++ name ++ "!")

        -- Thrown *weapons* (darts/javelins/tipped) clatter to the floor to be reclaimed; a fraction
        -- shatter on impact. A boomerang always returns. Lobbed potions/bombs always shatter.
        ( recovers, seedR ) =
            if def.id == "boomerang" then
                ( True, resolved.seed )

            else if isRecoverableThrow eff then
                Rng.chance 66 resolved.seed

            else
                ( False, resolved.seed )

        -- A returning boomerang flies back to the hero's hand rather than landing afield.
        landing =
            if def.id == "boomerang" then
                hero.pos

            else if Level.isPassableAt target resolved.level then
                target

            else
                hero.pos
    in
    (if recovers then
        { resolved | seed = seedR, items = { def = def, pos = landing } :: resolved.items }

     else
        { resolved | seed = seedR }
    )
        |> endTurn


{-| Thrown weapons that can be picked back up after they land (as opposed to shattering consumables). -}
isRecoverableThrow : ItemEffect -> Bool
isRecoverableThrow eff =
    case eff of
        ThrownHit _ ->
            True

        ThrownTipped _ _ ->
            True

        _ ->
            False


{-| Resolve a thrown effect bursting at `target`. -}
applyThrownEffect : ItemEffect -> Pos -> Game -> Game
applyThrownEffect eff target game =
    case eff of
        Incinerate _ ->
            spawnFire target game

        ToxicGas _ ->
            spawnGas CausticGasCloud 6 target game

        Freeze radius ->
            let
                chilled =
                    List.map
                        (\e ->
                            if Grid.chebyshev e.pos target <= radius then
                                { e | statuses = addEnemyStatus Crippled 1 5 (addEnemyStatus Slowed 1 4 e.statuses), alerted = True }

                            else
                                e
                        )
                        game.enemies
            in
            freezeWaterNear target { game | enemies = chilled }
                |> addLog "The flask bursts in a blast of frost!"

        ThrownHit power ->
            case enemyAt target game of
                Just e ->
                    let
                        hp =
                            e.hp - power
                    in
                    if hp <= 0 then
                        { game | enemies = List.filter (\x -> x.pos /= target) game.enemies, kills = game.kills + 1 }
                            |> addPopup target (String.fromInt power) "#d6deea"
                            |> gainXp e.def.xp
                            |> dropLoot e

                    else
                        { game | enemies = updateEnemyAt target (\x -> { x | hp = hp, alerted = True }) game.enemies }
                            |> addPopup target (String.fromInt power) "#d6deea"

                Nothing ->
                    game

        ThrownTipped power element ->
            case enemyAt target game of
                Just e ->
                    let
                        hp =
                            e.hp - power
                    in
                    if hp <= 0 then
                        { game | enemies = List.filter (\x -> x.pos /= target) game.enemies, kills = game.kills + 1 }
                            |> addPopup target (String.fromInt power) "#9be08a"
                            |> gainXp e.def.xp
                            |> dropLoot e

                    else
                        { game | enemies = updateEnemyAt target (\x -> { x | hp = hp, alerted = True, statuses = applyWandElement element x.statuses }) game.enemies }
                            |> addPopup target (String.fromInt power) "#9be08a"

                Nothing ->
                    game

        Explode dmg ->
            let
                hit e =
                    Grid.chebyshev e.pos target <= 1

                step e ( alive, xp, kills, pops ) =
                    if hit e then
                        let
                            hp =
                                e.hp - dmg
                        in
                        if hp <= 0 then
                            ( alive, xp + e.def.xp, kills + 1, { pos = e.pos, text = String.fromInt dmg, color = "#ff7a3c" } :: pops )

                        else
                            ( { e | hp = hp, alerted = True } :: alive, xp, kills, { pos = e.pos, text = String.fromInt dmg, color = "#ff7a3c" } :: pops )

                    else
                        ( e :: alive, xp, kills, pops )

                ( survivors, gained, killed, popups ) =
                    List.foldl step ( [], 0, 0, [] ) game.enemies

                -- A blast also catches the hero if adjacent, and leaves fire.
                heroHit =
                    if Grid.chebyshev game.hero.pos target <= 1 then
                        damageHero (dmg // 2) game |> addLog "The blast scorches you!"

                    else
                        game
            in
            { heroHit | enemies = List.reverse survivors, kills = heroHit.kills + killed, popups = popups ++ heroHit.popups }
                |> gainXp gained
                |> spawnFire target
                |> checkHeroDeath

        _ ->
            game


{-| Invoke an artifact: if it has reached full charge, apply its effect and reset it to empty;
otherwise it isn't ready yet (no turn spent). -}
useArtifact : Int -> Content.ArtifactSpec -> ItemDef -> Game -> Game
useArtifact index spec def game =
    if spec.charge < spec.maxCharge then
        addLog ("The " ++ def.name ++ " is still charging (" ++ String.fromInt spec.charge ++ "/" ++ String.fromInt spec.maxCharge ++ ").") game

    else
        let
            drained =
                { def | kind = Content.Artifact { spec | charge = 0 } }

            hero =
                game.hero
        in
        applyEffect def { game | hero = { hero | inventory = replaceAt index drained hero.inventory } }
            |> endTurn


{-| Zap the wand in inventory slot `index` at the nearest visible monster, spending a charge. A
drained wand (0 charges) just fizzles until it recharges. No turn is spent on a fizzle or no target. -}
zapWand : Int -> Content.WandSpec -> Game -> Game
zapWand index spec game =
    if spec.charges <= 0 then
        addLog "The wand is drained — wait for it to recharge." game

    else
        let
            ( misfire, seedM ) =
                Rng.chance 6 game.seed
        in
        if misfire then
            -- An unstable discharge: the bolt sputters and the charge is wasted.
            endTurn (spendCharge index spec { game | seed = seedM } |> addLog "The wand misfires — the spell fizzles!")

        else
            case nearestVisibleEnemy { game | seed = seedM } of
                Nothing ->
                    addLog "You wave the wand, but there is no target in sight." { game | seed = seedM }

                Just target ->
                    let
                        ( dmg, seed1 ) =
                            rollDamage spec.damage target.def.defense seedM

                        afterHit =
                            if target.hp - dmg <= 0 then
                                { game | enemies = List.filter (\e -> e.pos /= target.pos) game.enemies, seed = seed1, kills = game.kills + 1 }
                                    |> addLog ("Your bolt destroys the " ++ target.def.name ++ "!")
                                    |> addPopup target.pos (String.fromInt dmg) "#82aaff"
                                    |> gainXp target.def.xp
                                    |> dropLoot target

                            else
                                { game
                                    | enemies =
                                        updateEnemyAt target.pos
                                            (\e -> { e | hp = e.hp - dmg, alerted = True, statuses = applyWandElement spec.element e.statuses })
                                            game.enemies
                                    , seed = seed1
                                }
                                    |> addLog ("Your bolt hits the " ++ target.def.name ++ " (" ++ String.fromInt dmg ++ ").")
                                    |> addPopup target.pos (String.fromInt dmg) "#82aaff"
                                    |> wandChainOrSplash spec.element target.pos

                        hero =
                            afterHit.hero
                    in
                    endTurn (spendCharge index spec { afterHit | hero = hero })


{-| The status a wand's element inflicts on a struck monster. -}
applyWandElement : String -> List Status -> List Status
applyWandElement element statuses =
    case element of
        "fire" ->
            addEnemyStatus Burn 2 3 statuses

        "frost" ->
            addEnemyStatus Crippled 1 4 statuses

        "corrosion" ->
            addEnemyStatus Vulnerable 1 5 (addEnemyStatus Bleed 1 4 statuses)

        "regrowth" ->
            addEnemyStatus Crippled 1 5 statuses

        _ ->
            statuses


{-| A wand's secondary effect at the struck cell: lightning arcs to neighbours, a blast wave knocks the
target back, disintegration pierces every foe on the beam, regrowth entangles the area in grass. -}
wandChainOrSplash : String -> Pos -> Game -> Game
wandChainOrSplash element center game =
    case element of
        "shock" ->
            let
                arc e =
                    if not e.ally && Grid.chebyshev e.pos center == 1 then
                        { e | hp = e.hp - 3, alerted = True }

                    else
                        e
            in
            { game | enemies = List.map arc game.enemies } |> addLog "Lightning arcs to nearby foes!"

        "blast" ->
            knockBack center game |> addLog "The blast wave hurls your foe backward!"

        "disintegrate" ->
            let
                beam =
                    Grid.line game.hero.pos center

                onBeam e =
                    not e.ally && List.member e.pos beam

                hit e =
                    if onBeam e then
                        { e | hp = e.hp - 6, alerted = True }

                    else
                        e
            in
            { game | enemies = List.map hit game.enemies } |> addLog "The beam pierces everything in its path!"

        "regrowth" ->
            let
                grown =
                    List.foldl
                        (\p lv ->
                            if Level.at p lv == Floor then
                                Level.set p Grass lv

                            else
                                lv
                        )
                        game.level
                        (cellsWithin 1 center)
            in
            { game | level = grown } |> addLog "Grass erupts and entangles the area!"

        "frost" ->
            freezeWaterNear center game

        "warding" ->
            let
                warded e =
                    if not e.ally && Grid.chebyshev e.pos center <= 1 then
                        { e | statuses = addEnemyStatus Paralyzed 1 3 e.statuses, alerted = True }

                    else
                        e
            in
            { game | enemies = List.map warded game.enemies } |> addLog "Glyphs of warding freeze your foes in place!"

        "transfusion" ->
            let
                hero =
                    game.hero

                heal =
                    6 + game.depth // 2
            in
            { game | hero = { hero | hp = min hero.maxHp (hero.hp + heal) } }
                |> addPopup game.hero.pos ("+" ++ String.fromInt heal) "#5dd47a"
                |> addLog "Life flows from your foe into you!"

        _ ->
            game


{-| Shove the monster at `pos` one cell directly away from the hero, if that cell is free. -}
knockBack : Pos -> Game -> Game
knockBack pos game =
    case enemyAt pos game of
        Just e ->
            let
                dx =
                    clamp -1 1 (pos.x - game.hero.pos.x)

                dy =
                    clamp -1 1 (pos.y - game.hero.pos.y)

                dest =
                    { x = pos.x + dx, y = pos.y + dy }

                occupied =
                    enemyAt dest game /= Nothing || dest == game.hero.pos
            in
            if Level.at dest game.level == Chasm && not occupied && not e.ally then
                -- Hurled over the edge: the foe plunges into the chasm and is gone.
                { game | enemies = List.filter (\x -> x.pos /= pos) game.enemies, kills = game.kills + 1 }
                    |> gainXp e.def.xp
                    |> addLog ("The " ++ e.def.name ++ " is hurled screaming into the chasm!")

            else if Level.isPassableAt dest game.level && not occupied then
                { game | enemies = updateEnemyAt pos (\x -> { x | pos = dest }) game.enemies }

            else
                game

        Nothing ->
            game


{-| Decrement a wand's charge in the inventory after a zap, keeping the item's other fields. -}
spendCharge : Int -> Content.WandSpec -> Game -> Game
spendCharge index spec game =
    let
        hero =
            game.hero
    in
    { game
        | hero =
            { hero
                | inventory =
                    List.indexedMap
                        (\i it ->
                            if i == index then
                                { it | kind = Content.Wand { spec | charges = spec.charges - 1 } }

                            else
                                it
                        )
                        hero.inventory
            }
    }


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
        |> List.filter (\e -> not e.ally && Set.member ( e.pos.x, e.pos.y ) game.visible)
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

        -- Fold any max-HP bonus from the gear into the hero's pool (delta vs the piece swapped out).
        maxHpDelta =
            equipBonus .maxHp (Just def) - equipBonus .maxHp previous

        withVitality h =
            { h | maxHp = max 1 (h.maxHp + maxHpDelta), hp = max 1 (h.hp + maxHpDelta) }

        equippedHero =
            case slot of
                Content.WeaponSlot ->
                    withVitality { hero | inventory = pack, weapon = Just def }

                Content.ArmourSlot ->
                    withVitality { hero | inventory = pack, armour = Just def }

                Content.RingSlot ->
                    withVitality { hero | inventory = pack, ring = Just def }

        cursedNote =
            if isCursed def then
                " It's cursed — it won't come off!"

            else
                ""
    in
    case previous of
        Just old ->
            if isCursed old then
                addLog ("You can't remove the cursed " ++ old.name ++ "!") game

            else
                { game | hero = equippedHero } |> addLog ("You equip the " ++ def.name ++ "." ++ cursedNote)

        Nothing ->
            { game | hero = equippedHero } |> addLog ("You equip the " ++ def.name ++ "." ++ cursedNote)


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
            if List.member "no-healing" game.challenges then
                addLog ("You drink the " ++ name ++ ", but your phobia turns the healing to ash.") game

            else
                cureDebuffs { game | hero = { hero | hp = min hero.maxHp (hero.hp + n) } }
                    |> addLog ("You drink the " ++ name ++ ". (+" ++ String.fromInt n ++ " HP, ailments soothed)")

        HealFull ->
            if List.member "no-healing" game.challenges then
                addLog ("You drink the " ++ name ++ ", but your phobia turns the healing to ash.") game

            else
                cureDebuffs { game | hero = { hero | hp = hero.maxHp } }
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

        Recharge ->
            { game | hero = { hero | inventory = List.map fullyCharge hero.inventory } }
                |> addLog "You read the scroll. Your wands hum, fully recharged."

        Terror ->
            let
                routed =
                    List.map
                        (\e ->
                            if Set.member ( e.pos.x, e.pos.y ) game.visible then
                                { e | fleeing = True, alerted = True }

                            else
                                e
                        )
                        game.enemies
            in
            { game | enemies = routed }
                |> addLog "You read the scroll. Nearby monsters flee in terror!"

        RemoveCurse ->
            { game
                | hero =
                    { hero
                        | weapon = Maybe.map uncurse hero.weapon
                        , armour = Maybe.map uncurse hero.armour
                        , ring = Maybe.map uncurse hero.ring
                    }
            }
                |> addLog "You read the scroll. A malign weight lifts from your gear."

        Invisibility turns ->
            addStatus Invisible 1 turns game
                |> addLog ("You drink the " ++ name ++ ". You fade from sight.")

        Levitation turns ->
            addStatus Levitating 1 turns game
                |> addLog ("You drink the " ++ name ++ ". You drift off the ground.")

        MindVision ->
            let
                minds =
                    Set.fromList (List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies)
            in
            { game | explored = Set.union game.explored minds }
                |> addLog ("You drink the " ++ name ++ ". You sense the minds around you.")

        Experience xp ->
            gainXp xp game
                |> addLog ("You drink the " ++ name ++ ". Knowledge floods in.")

        Incinerate _ ->
            spawnFire game.hero.pos game
                |> addLog ("The " ++ name ++ " shatters and bursts into flame!")

        ToxicGas _ ->
            spawnGas CausticGasCloud 6 game.hero.pos game
                |> addLog ("The " ++ name ++ " shatters into a cloud of caustic gas!")

        Lullaby turns ->
            let
                lulled =
                    List.map
                        (\e ->
                            if Set.member ( e.pos.x, e.pos.y ) game.visible then
                                { e | alerted = False, fleeing = False, statuses = addEnemyStatus Paralyzed 1 turns e.statuses }

                            else
                                e
                        )
                        game.enemies
            in
            { game | enemies = lulled }
                |> addLog "You read the scroll. A soothing melody lulls the nearby monsters to sleep."

        Retribution magnitude ->
            psionicBlast (magnitude + game.depth) game
                |> addLog "You read the scroll. A wave of force erupts outward!"

        Transmute ->
            transmuteItem game

        GrowGrass ->
            let
                grown =
                    List.foldl
                        (\p lv ->
                            if Level.at p lv == Floor then
                                Level.set p Grass lv

                            else
                                lv
                        )
                        game.level
                        (cellsWithin 2 game.hero.pos)
            in
            { game | level = grown }
                |> addLog "You read the scroll. Tall grass bursts from the ground around you."

        Aggravate ->
            { game | enemies = List.map (\e -> { e | alerted = True }) game.enemies }
                |> addLog "You read the scroll. A blaring note rouses every monster on the floor!"

        Corrupt ->
            case nearestVisibleEnemy game of
                Just target ->
                    { game | enemies = updateEnemyAt target.pos (\e -> { e | ally = True, fleeing = False, alerted = True }) game.enemies }
                        |> addLog ("The " ++ target.def.name ++ " is corrupted — it now fights at your side!")

                Nothing ->
                    addLog "You read the scroll, but there is no foe in sight to corrupt." game

        Enchant ->
            case ( hero.weapon, hero.armour ) of
                ( Just _, _ ) ->
                    -- A weapon present: let the player pick the enchantment (resolved by chooseEnchant).
                    { game | awaitingEnchant = True }
                        |> addLog "Runes swirl above your weapon — choose an enchantment."

                ( Nothing, Just _ ) ->
                    { game | hero = { hero | armour = Maybe.map (setEnchant "thorns") hero.armour } }
                        |> addLog "Runes blaze across your armour — it now bears thorns."

                ( Nothing, Nothing ) ->
                    addLog "You read the scroll, but have no gear to enchant." game

        SummonAlly ->
            let
                free =
                    Grid.neighbors8 game.hero.pos
                        |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing)
                        |> List.head
            in
            case free of
                Just spot ->
                    { game | enemies = { def = ghostAllyDef game.depth, pos = spot, hp = ghostAllyDef game.depth |> .maxHp, alerted = True, fleeing = False, statuses = [], ally = True, revives = 0 } :: game.enemies }
                        |> addLog "A spectral ally rises to fight beside you!"

                Nothing ->
                    addLog "There is no room for an ally to appear." game

        ReleaseBees ->
            let
                free =
                    Grid.neighbors8 game.hero.pos
                        |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing)
                        |> List.take 2

                bees =
                    List.map (\spot -> { def = beeAllyDef game.depth, pos = spot, hp = beeAllyDef game.depth |> .maxHp, alerted = True, fleeing = False, statuses = [], ally = True, revives = 0 }) free
            in
            if List.isEmpty bees then
                addLog "The honeypot shatters, but the bees find no room and disperse." game

            else
                { game | enemies = game.enemies ++ bees }
                    |> addLog "The honeypot shatters — angry bees swarm to your defense!"

        Foresight radius ->
            let
                origin =
                    game.hero.pos

                inRange =
                    Level.positions game.level
                        |> List.filter (\p -> Grid.chebyshev p origin <= radius)

                ( revealedLevel, found ) =
                    List.foldl
                        (\p ( lv, n ) ->
                            if Level.at p lv == SecretDoor then
                                ( Level.set p Door lv, n + 1 )

                            else
                                ( lv, n )
                        )
                        ( game.level, 0 )
                        inRange
            in
            { game
                | level = revealedLevel
                , explored = Set.union game.explored (Set.fromList (List.map (\p -> ( p.x, p.y )) inRange))
            }
                |> addLog
                    (if found > 0 then
                        "Foresight floods your mind — you sense the surroundings and " ++ String.fromInt found ++ " hidden door(s)."

                     else
                        "Foresight floods your mind — you sense the surroundings."
                    )

        PullNearest ->
            case nearestVisibleEnemy game of
                Just target ->
                    let
                        spot =
                            Grid.neighbors8 game.hero.pos
                                |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing && p /= game.hero.pos)
                                |> List.head
                                |> Maybe.withDefault target.pos
                    in
                    { game | enemies = updateEnemyAt target.pos (\e -> { e | pos = spot, alerted = True }) game.enemies }
                        |> addLog ("Ethereal chains yank the " ++ target.def.name ++ " to your side!")

                Nothing ->
                    addLog "The chains find no target." game

        RandomScroll ->
            let
                scrolls =
                    game.ruleset.items |> List.filter isScroll

                ( picked, seed1 ) =
                    case scrolls of
                        first :: _ ->
                            Rng.pick first scrolls game.seed

                        [] ->
                            ( def, game.seed )
            in
            applyEffect picked { game | seed = seed1 }
                |> addLog "The unstable spellbook discharges a random spell!"

        Cleanse ->
            cureDebuffs { game | gas = Dict.empty, fire = Dict.empty }
                |> addLog ("You drink the " ++ name ++ ". Ailments fade and the air around you clears.")

        ChargeAbility ->
            { game | hero = { hero | abilityCharge = abilityMax } }
                |> addLog "You read the scroll. Your class ability surges to full charge."

        Shield amount ->
            addStatus Shielded amount 20 game
                |> addLog ("A shimmering barrier wraps around you, soaking the next " ++ String.fromInt amount ++ " damage.")

        Polymorph ->
            case nearestVisibleEnemy game of
                Just target ->
                    { game
                        | enemies =
                            updateEnemyAt target.pos
                                (\e -> { e | def = sheepDef, hp = sheepDef.maxHp, ally = False, alerted = False, fleeing = False, statuses = [] })
                                game.enemies
                    }
                        |> addLog ("The " ++ target.def.name ++ " is transformed into a docile sheep!")

                Nothing ->
                    addLog "You read the scroll, but there is no creature in sight to transform." game

        Blink ->
            case nearestVisibleEnemy game of
                Just target ->
                    let
                        spot =
                            Grid.neighbors8 target.pos
                                |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing && p /= game.hero.pos)
                                |> List.sortBy (\p -> Grid.chebyshev p game.hero.pos)
                                |> List.head
                    in
                    case spot of
                        Just p ->
                            refreshFov { game | hero = { hero | pos = p } }
                                |> addLog "Reality folds — you blink to your quarry's side!"

                        Nothing ->
                            addLog "You read the scroll, but there is no room beside your target." game

                Nothing ->
                    addLog "You read the scroll, but no foe is in sight to blink toward." game

        SummonDecoy ->
            let
                free =
                    Grid.neighbors8 game.hero.pos
                        |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing)
                        |> List.head
            in
            case free of
                Just spot ->
                    { game | enemies = { def = sheepDef, pos = spot, hp = sheepDef.maxHp, alerted = True, fleeing = False, statuses = [], ally = True, revives = 0 } :: game.enemies }
                        |> addLog "A sheep decoy bleats into being, drawing your foes' attention!"

                Nothing ->
                    addLog "There is no room for a decoy to appear." game

        PlantSeed kindName ->
            case plantFromName kindName of
                Just kind ->
                    { game | plants = Dict.insert ( hero.pos.x, hero.pos.y ) kind game.plants }
                        |> addLog ("You sow the seed; " ++ kindName ++ " sprouts at your feet.")

                Nothing ->
                    addLog "The seed refuses to take root here." game


{-| Map a seed/plant name to its `PlantKind` (for sowing seeds). -}
plantFromName : String -> Maybe PlantKind
plantFromName name =
    case name of
        "firebloom" ->
            Just Firebloom

        "sungrass" ->
            Just Sungrass

        "sorrowmoss" ->
            Just Sorrowmoss

        "earthroot" ->
            Just Earthroot

        _ ->
            Nothing


{-| Damage every monster the hero can see (a psionic blast / scroll of retribution). -}
psionicBlast : Int -> Game -> Game
psionicBlast power game =
    let
        step e ( alive, xp, kills, pops ) =
            if Set.member ( e.pos.x, e.pos.y ) game.visible then
                let
                    hp =
                        e.hp - power
                in
                if hp <= 0 then
                    ( alive, xp + e.def.xp, kills + 1, { pos = e.pos, text = String.fromInt power, color = "#c9a0ff" } :: pops )

                else
                    ( { e | hp = hp, alerted = True } :: alive, xp, kills, { pos = e.pos, text = String.fromInt power, color = "#c9a0ff" } :: pops )

            else
                ( e :: alive, xp, kills, pops )

        ( survivors, gained, killed, popups ) =
            List.foldl step ( [], 0, 0, [] ) game.enemies
    in
    { game | enemies = List.reverse survivors, kills = game.kills + killed, popups = popups ++ game.popups }
        |> gainXp gained


{-| Reroll one random non-equipped pack item into another of the same kind (scroll of transmutation). -}
transmuteItem : Game -> Game
transmuteItem game =
    let
        hero =
            game.hero
    in
    case hero.inventory of
        [] ->
            addLog "You read the scroll, but have nothing to transmute." game

        _ ->
            let
                ( idx, seed1 ) =
                    Rng.int (List.length hero.inventory) game.seed

                target =
                    nth idx hero.inventory

                sameKind a b =
                    sameItemKind a.kind b.kind
            in
            case target of
                Nothing ->
                    { game | seed = seed1 }

                Just old ->
                    let
                        pool =
                            Content.itemsForDepth game.depth game.ruleset
                                |> List.filter (\( _, d ) -> sameKind d old && d.id /= old.id)
                    in
                    case pool of
                        ( _, firstDef ) :: _ ->
                            let
                                ( fresh, seed2 ) =
                                    Rng.pickWeighted firstDef pool seed1
                            in
                            { game
                                | seed = seed2
                                , hero = { hero | inventory = replaceAt idx fresh hero.inventory }
                            }
                                |> identify fresh
                                |> addLog ("You read the scroll. Your " ++ old.name ++ " becomes " ++ withArticle (displayName game.idents fresh) ++ "!")

                        [] ->
                            { game | seed = seed1 }
                                |> addLog "You read the scroll, but nothing changes."


{-| Two item kinds count as the same category for transmutation (consumable↔consumable, etc.). -}
sameItemKind : Content.ItemKind -> Content.ItemKind -> Bool
sameItemKind a b =
    case ( a, b ) of
        ( Content.Consumable _, Content.Consumable _ ) ->
            True

        ( Content.Equipment sa _, Content.Equipment sb _ ) ->
            sa == sb

        ( Content.Wand _, Content.Wand _ ) ->
            True

        _ ->
            False


effectOf : ItemDef -> ItemEffect
effectOf def =
    case def.kind of
        Content.Consumable eff ->
            eff

        Content.Artifact spec ->
            spec.effect

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


{-| Set the glyph-enchantment on an equipment item (scroll of enchantment / reforging). -}
setEnchant : String -> ItemDef -> ItemDef
setEnchant ench item =
    case item.kind of
        Content.Equipment slot bonus ->
            { item | kind = Content.Equipment slot { bonus | enchant = ench } }

        _ ->
            item


{-| Return a copy of an equipment item with its enchantment level raised by one (also lifts a curse). -}
enchant : ItemDef -> ItemDef
enchant item =
    case item.kind of
        Content.Equipment slot bonus ->
            { item | kind = Content.Equipment slot { bonus | plus = bonus.plus + 1, cursed = False } }

        _ ->
            item


{-| Refill a wand in the pack to its maximum charges (others untouched). -}
fullyCharge : ItemDef -> ItemDef
fullyCharge item =
    case item.kind of
        Content.Wand spec ->
            { item | kind = Content.Wand { spec | charges = spec.maxCharges } }

        _ ->
            item


{-| Clear the curse on a piece of equipment. -}
uncurse : ItemDef -> ItemDef
uncurse item =
    case item.kind of
        Content.Equipment slot bonus ->
            { item | kind = Content.Equipment slot { bonus | cursed = False } }

        _ ->
            item


{-| Strip the curse flag from a piece of gear (used by altars / remove-curse). -}
uncurseItem : ItemDef -> ItemDef
uncurseItem item =
    case item.kind of
        Content.Equipment slot bonus ->
            if bonus.cursed then
                { item | kind = Content.Equipment slot { bonus | cursed = False } }

            else
                item

        _ ->
            item


-- IDENTIFICATION ---------------------------------------------------------------------------------


{-| A discovery-journal projection of the ruleset's items for the catalog UI. Each entry carries the
masked display name/colour (so unidentified consumables stay secret), a category bucket for grouping,
and whether the hero has identified it this run. Items that are never disguised count as always known. -}
itemCatalog : Game -> List { glyph : String, name : String, color : String, category : String, known : Bool }
itemCatalog game =
    let
        category def =
            if isPotion def then
                "Potions"

            else if isScroll def then
                "Scrolls"

            else if isWand def then
                "Wands"

            else if isRing def then
                "Rings"

            else
                case def.kind of
                    Content.Artifact _ ->
                        "Artifacts"

                    Content.Equipment Content.WeaponSlot _ ->
                        "Weapons"

                    Content.Equipment Content.ArmourSlot _ ->
                        "Armour"

                    _ ->
                        "Other"

        entry def =
            { glyph = def.glyph
            , name = displayName game.idents def
            , color = displayColor game.idents def
            , category = category def
            , known = not (unidentifiable def) || Set.member def.id game.idents.known
            }
    in
    List.map entry game.ruleset.items


{-| Mark a potion or scroll identified (after use), announcing what it was if newly learned. -}
identify : ItemDef -> Game -> Game
identify def game =
    if unidentifiable def && not (Set.member def.id game.idents.known) then
        let
            idents =
                game.idents
        in
        { game | idents = { idents | known = Set.insert def.id idents.known } }
            |> addLog ("It was " ++ withArticle def.name ++ "!")

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
            -- A levitating hero floats over pressure plates without setting them off.
            if hasStatus Levitating game.hero then
                game

            else
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
            spawnGas ToxicGasCloud 5 game.hero.pos game
                |> addLog "Toxic gas billows out of the floor!"

        TeleportTrap ->
            teleportHero game |> addLog "A teleport trap! You are flung across the floor."

        ParalysisTrap ->
            addStatus Paralyzed 1 4 game |> addLog "A paralysis trap! Your limbs lock up."

        RockfallTrap ->
            let
                ( dmg, s1 ) =
                    Rng.range (game.depth + 3) (game.depth + 7) game.seed
            in
            checkHeroDeath (damageHero dmg { game | seed = s1 } |> addLog ("Rocks crash down! (" ++ String.fromInt dmg ++ ")"))

        AlarmTrap ->
            { game | enemies = List.map (\e -> { e | alerted = True }) game.enemies }
                |> addLog "An alarm blares — every monster on the floor is roused!"

        FrostTrap ->
            addStatus Slowed 1 8 game |> addLog "A frost trap! Ice slows your movements."

        FlameTrap ->
            spawnFire game.hero.pos game |> addLog "A flame trap roars to life beneath you!"

        SummonTrap ->
            let
                candidates =
                    Content.enemiesForDepth game.depth game.ruleset

                free =
                    Grid.neighbors8 game.hero.pos
                        |> List.filter (\p -> Level.isPassableAt p game.level && enemyAt p game == Nothing)
                        |> List.take 2

                ( picks, s1 ) =
                    List.foldl
                        (\spot ( acc, s ) ->
                            case candidates of
                                ( _, firstDef ) :: _ ->
                                    let
                                        ( def, s2 ) =
                                            Rng.pickWeighted firstDef candidates s
                                    in
                                    ( { def = def, pos = spot, hp = def.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 } :: acc, s2 )

                                [] ->
                                    ( acc, s )
                        )
                        ( [], game.seed )
                        free
            in
            if List.isEmpty picks then
                addLog "A summoning rune flickers but fizzles." game

            else
                { game | enemies = game.enemies ++ picks, seed = s1 }
                    |> addLog "A summoning trap! Monsters claw out of the floor!"

        PitfallTrap ->
            fallThroughChasm game


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

        -- Searching also tries to disarm adjacent (now-revealed) traps — each ~55% to dismantle.
        ( keptTraps, disarmed, seed1 ) =
            List.foldl
                (\t ( kept, n, s ) ->
                    if near t.pos && t.revealed then
                        let
                            ( roll, s2 ) =
                                Rng.chance 55 s
                        in
                        if roll then
                            ( kept, n + 1, s2 )

                        else
                            ( t :: kept, n, s2 )

                    else
                        ( t :: kept, n, s )
                )
                ( [], 0, game.seed )
                revealedTraps

        total =
            trapsFound + doorsFound
    in
    { game | traps = List.reverse keptTraps, level = level1, seed = seed1 }
        |> addLog
            (if disarmed > 0 then
                "You find and disarm " ++ String.fromInt disarmed ++ " trap(s)."

             else if total > 0 then
                "You find " ++ String.fromInt total ++ " hidden thing(s) nearby!"

             else
                "You search but find nothing."
            )


