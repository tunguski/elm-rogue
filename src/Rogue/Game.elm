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
    , subclass : Maybe String
    , talents : List String
    , heroClass : String
    , abilityCharge : Int
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
    | Invisible
    | Levitating
    | Bleed
    | Weakened
    | Vulnerable
    | Crippled
    | Shielded
    | Charmed


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

                Invisible ->
                    "Invisible"

                Levitating ->
                    "Levitating"

                Bleed ->
                    "Bleed"

                Weakened ->
                    "Weakened"

                Vulnerable ->
                    "Vulnerable"

                Crippled ->
                    "Crippled"

                Shielded ->
                    "Shield " ++ String.fromInt status.magnitude

                Charmed ->
                    "Charmed"
    in
    name ++ " (" ++ String.fromInt status.turns ++ ")"


{-| A pip colour for an over-actor status icon — harmful reds/greens, helpful blues/golds. "" hides it. -}
statusColor : StatusKind -> String
statusColor kind =
    case kind of
        Poison ->
            "#7fd06a"

        Burn ->
            "#e0834b"

        Bleed ->
            "#d04a3a"

        Regen ->
            "#5dd47a"

        Lit ->
            "#ecd24c"

        Paralyzed ->
            "#c9a23a"

        Hasted ->
            "#5dd4cf"

        Slowed ->
            "#82aaff"

        Invisible ->
            "#aab4c8"

        Levitating ->
            "#9be0ff"

        Weakened ->
            "#9a6ad8"

        Vulnerable ->
            "#e07ab0"

        Crippled ->
            "#b88a4a"

        Shielded ->
            "#82c8ff"

        Charmed ->
            "#e07ab0"


hasStatus : StatusKind -> Hero -> Bool
hasStatus kind hero =
    List.any (\s -> s.kind == kind) hero.statuses


{-| The hero's attack power including the worn weapon's and ring's bonuses, plus subclass/talent
modifiers (the Duellist's flat bonus, the Berserker's low-HP rage, Sharpened-Edge talent). -}
heroDamage : Hero -> Int
heroDamage hero =
    let
        sub =
            case hero.subclass of
                Just "Duellist" ->
                    2

                Just "Berserker" ->
                    if hero.hp * 3 <= hero.maxHp then
                        4

                    else
                        0

                _ ->
                    0

        talent =
            (if List.member "Sharpened Edge" hero.talents then
                1

             else
                0
            )
                + (if List.member "Deadly Strike" hero.talents then
                    2

                   else
                    0
                  )
                + (if List.member "Heroism" hero.talents then
                    3

                   else
                    0
                  )

        weak =
            if hasStatus Weakened hero then
                -3

            else
                0
    in
    max 1 (hero.damage + equipBonus .damage hero.weapon + equipBonus .damage hero.ring + sub + talent + weak)


{-| The hero's defense including the worn armour's, ring's and subclass/talent bonuses. -}
heroDefense : Hero -> Int
heroDefense hero =
    let
        sub =
            if hero.subclass == Just "Sentinel" then
                2

            else
                0

        talent =
            (if List.member "Iron Will" hero.talents then
                1

             else
                0
            )
                + (if List.member "Bulwark" hero.talents then
                    3

                   else
                    0
                  )
    in
    hero.defense + equipBonus .defense hero.armour + equipBonus .defense hero.ring + sub + talent


{-| The enchantment id on an equipped item (weapon or armour), or "" if none / unenchanted. -}
itemEnchant : Maybe ItemDef -> String
itemEnchant maybeItem =
    case maybeItem of
        Just it ->
            case it.kind of
                Content.Equipment _ bonus ->
                    bonus.enchant

                _ ->
                    ""

        Nothing ->
            ""


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
    , ally : Bool
    , revives : Int
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


{-| A locked chest: bump it with a key to claim its loot — unless it's a `mimic`, which springs into a
monster when opened. -}
type alias Chest =
    { pos : Pos
    , loot : ItemDef
    , mimic : Bool
    , crystal : Bool
    }


{-| A magic well: drink from it once by stepping on it. `Health` fully heals and adds vitality,
`Awareness` maps the floor, `Transmutation` rerolls a random carried item. -}
type WellKind
    = HealthWell
    | AwarenessWell
    | TransmuteWell


type alias Well =
    { pos : Pos
    , kind : WellKind
    }


{-| A friendly NPC you bump to talk to. `Ghost` gifts an item; `Sage` identifies potions; `Wandmaker`
hands over a wand; `Blacksmith` reforges your weapon (+1); `Imp` strikes a bounty bargain. -}
type NpcKind
    = Ghost
    | Sage
    | Wandmaker
    | Blacksmith
    | Imp


type alias Npc =
    { pos : Pos
    , kind : NpcKind
    , reward : ItemDef
    }


{-| An accepted bounty (the imp's quest): slay monsters until `targetKills`, then the reward is
delivered automatically. Carried across floors. -}
type alias Quest =
    { targetKills : Int
    , targetDepth : Int
    , reward : ItemDef
    , giver : String
    }


{-| What a previous hero left behind on dying: gold and one item, recovered on reaching that depth. -}
type alias Remains =
    { depth : Int
    , gold : Int
    , itemId : String
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
    | RockfallTrap
    | AlarmTrap
    | FrostTrap
    | FlameTrap
    | SummonTrap
    | PitfallTrap


type alias Trap =
    { pos : Pos
    , kind : TrapKind
    , revealed : Bool
    }


{-| A growing plant on a floor cell. Stepping on it triggers its effect and consumes it: `Firebloom`
ignites, `Sungrass` heals, `Sorrowmoss` poisons, `Earthroot` roots (paralyses) the stepper. -}
type PlantKind
    = Firebloom
    | Sungrass
    | Sorrowmoss
    | Earthroot


{-| A volatile gas occupying a cell: it spreads to neighbours and thins by one each turn. -}
type GasKind
    = ToxicGasCloud
    | CausticGasCloud
    | ParalyticGasCloud


type alias Gas =
    { kind : GasKind
    , density : Int
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
    , chests : List Chest
    , altar : Maybe Pos
    , well : Maybe Well
    , statues : List Pos
    , npc : Maybe Npc
    , quest : Maybe Quest
    , challenges : List String
    , remains : Maybe Remains
    , ascending : Bool
    , awaitingGhostGift : Bool
    , awaitingEnchant : Bool
    , popups : List Popup
    , traps : List Trap
    , gas : Dict ( Int, Int ) Gas
    , fire : Dict ( Int, Int ) Int
    , ice : Dict ( Int, Int ) Int
    , plants : Dict ( Int, Int ) PlantKind
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
    12


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


{-| A compact, serialisable snapshot of a run: the hero's progression and gear (gear/inventory by item
id, re-resolved against the ruleset on load) plus the floor depth and a seed. Enough to continue a
character at the start of a freshly generated floor at the same depth. -}
type alias SaveData =
    { depth : Int
    , hp : Int
    , maxHp : Int
    , damage : Int
    , defense : Int
    , gold : Int
    , level : Int
    , xp : Int
    , nutrition : Int
    , fovRadius : Int
    , glyph : String
    , color : String
    , weaponId : Maybe String
    , armourId : Maybe String
    , ringId : Maybe String
    , inventoryIds : List String
    , knownIds : List String
    , seed : Int
    }


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
    in
    { game | challenges = ids, hero = stripped }


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
    ]


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
            Fov.compute (fovRadiusFor heroAt depth) heroAt.pos gen.level
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
animateStatues : Game -> Game
animateStatues game =
    let
        ( woken, dormant ) =
            List.partition (\p -> Grid.chebyshev p game.hero.pos <= 2) game.statues
    in
    if List.isEmpty woken then
        game

    else
        let
            guards =
                List.map
                    (\p ->
                        let
                            def =
                                guardianDef game.depth
                        in
                        { def = def, pos = p, hp = def.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
                    )
                    woken
        in
        { game | statues = dormant, enemies = guards ++ game.enemies }
            |> addLog "A statue grinds to life — a guardian attacks!"


guardianDef : Int -> EnemyDef
guardianDef depth =
    { id = "guardian"
    , name = "animated guardian"
    , glyph = "8"
    , color = "#b0b0b8"
    , maxHp = 26 + depth * 3
    , damage = 7 + depth // 2
    , defense = 5
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 8 + depth
    , drop = Nothing
    }


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
            endTurn (heroAttack enemy game)

        Nothing ->
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


isKey : ItemDef -> Bool
isKey def =
    def.kind == Content.Key


{-| A compact HUD summary of the keyring — keys grouped by name with counts (e.g. "iron key ×2 ·
crystal key ×1"). Empty when the hero carries none; keys live here instead of cluttering the bag. -}
keyringLabel : List ItemDef -> String
keyringLabel inventory =
    let
        names =
            inventory |> List.filter isKey |> List.map .name

        count name =
            List.length (List.filter ((==) name) names)

        uniques =
            names |> List.foldl (\n acc -> if List.member n acc then acc else acc ++ [ n ]) []
    in
    uniques
        |> List.map (\n -> n ++ " ×" ++ String.fromInt (count n))
        |> String.join " · "


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


{-| Turns to fully charge a class ability. -}
abilityMax : Int
abilityMax =
    40


abilityName : String -> String
abilityName classId =
    case classId of
        "warrior" ->
            "Ground Slam"

        "mage" ->
            "Elemental Blast"

        "rogue" ->
            "Smoke Bomb"

        "huntress" ->
            "Spectral Blades"

        "duelist" ->
            "Lunge"

        _ ->
            "Ability"


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
        -- shatter on impact. Lobbed potions/bombs always shatter and leave nothing.
        ( recovers, seedR ) =
            if isRecoverableThrow eff then
                Rng.chance 66 resolved.seed

            else
                ( False, resolved.seed )

        landing =
            if Level.isPassableAt target resolved.level then
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


cellsWithin : Int -> Pos -> List Pos
cellsWithin r center =
    List.concatMap
        (\dy -> List.map (\dx -> { x = center.x + dx, y = center.y + dy }) (List.range -r r))
        (List.range -r r)


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


isCursed : ItemDef -> Bool
isCursed item =
    case item.kind of
        Content.Equipment _ bonus ->
            bonus.cursed

        _ ->
            False



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
    , { adjective = "inky", color = "#6a6f86" }
    , { adjective = "milky", color = "#d6d2c2" }
    , { adjective = "bubbling", color = "#6ad8c0" }
    , { adjective = "charcoal", color = "#4a4f5e" }
    , { adjective = "cloudy", color = "#aeb6c2" }
    , { adjective = "ivory", color = "#e6e0cf" }
    ]


{-| The pool of runic labels an unidentified scroll can wear this run. -}
scrollPalette : List Appearance
scrollPalette =
    [ { adjective = "GORO", color = "#c9b88a" }
    , { adjective = "KAUNAN", color = "#caa472" }
    , { adjective = "OYEE", color = "#b8c46a" }
    , { adjective = "ZID", color = "#9ab0d6" }
    , { adjective = "TIWAZ", color = "#d4a06a" }
    , { adjective = "ELAR", color = "#a7c46a" }
    , { adjective = "VARK", color = "#c79ad6" }
    , { adjective = "WERG", color = "#d6c27a" }
    , { adjective = "NYX", color = "#9aa7ba" }
    , { adjective = "RETH", color = "#caa0a0" }
    , { adjective = "MOTH", color = "#b8c0a0" }
    , { adjective = "QORN", color = "#c0a0c8" }
    , { adjective = "FENG", color = "#a0c0c0" }
    , { adjective = "ULAR", color = "#c8b890" }
    , { adjective = "DROV", color = "#b0a8c8" }
    , { adjective = "SKAL", color = "#c8a890" }
    , { adjective = "BRIX", color = "#a0c8b0" }
    , { adjective = "VOTH", color = "#c8c090" }
    ]


{-| Is this item a potion (and so subject to identification)? -}
isPotion : ItemDef -> Bool
isPotion def =
    case def.kind of
        Content.Consumable _ ->
            String.startsWith "potion" def.id

        _ ->
            False


{-| Is this item a scroll? -}
isScroll : ItemDef -> Bool
isScroll def =
    case def.kind of
        Content.Consumable _ ->
            String.startsWith "scroll" def.id

        _ ->
            False


isRing : ItemDef -> Bool
isRing def =
    case def.kind of
        Content.Equipment Content.RingSlot _ ->
            True

        _ ->
            False


isWand : ItemDef -> Bool
isWand def =
    case def.kind of
        Content.Wand _ ->
            True

        _ ->
            False


{-| The gem/wood labels unidentified rings and wands wear this run. -}
ringPalette : List Appearance
ringPalette =
    [ { adjective = "diamond", color = "#9be0ff" }
    , { adjective = "ruby", color = "#e0564b" }
    , { adjective = "emerald", color = "#5dd47a" }
    , { adjective = "topaz", color = "#d8b24c" }
    , { adjective = "agate", color = "#c79ad6" }
    , { adjective = "onyx", color = "#9aa7ba" }
    , { adjective = "sapphire", color = "#4f8bff" }
    , { adjective = "garnet", color = "#caa0a0" }
    ]


wandPalette : List Appearance
wandPalette =
    [ { adjective = "yew", color = "#caa472" }
    , { adjective = "ebony", color = "#6a6f86" }
    , { adjective = "birch", color = "#d6d2c2" }
    , { adjective = "holly", color = "#5dd47a" }
    , { adjective = "willow", color = "#a7c46a" }
    , { adjective = "rowan", color = "#e0824b" }
    , { adjective = "teak", color = "#b8895a" }
    , { adjective = "elm", color = "#9ab0d6" }
    , { adjective = "alder", color = "#c0a060" }
    , { adjective = "cedar", color = "#d08a5a" }
    ]


{-| Items subject to per-run identification (potions, scrolls, rings and wands). -}
unidentifiable : ItemDef -> Bool
unidentifiable def =
    isPotion def || isScroll def || isRing def || isWand def


{-| Assign each randomized item id a distinct appearance for the run. -}
assignLooks : Ruleset -> Seed -> ( Dict String Appearance, Seed )
assignLooks ruleset seed =
    let
        idsOf pred =
            ruleset.items |> List.filter pred |> List.map .id

        ( potionLooks, seed1 ) =
            Rng.shuffle palette seed

        ( scrollLooks, seed2 ) =
            Rng.shuffle scrollPalette seed1

        ( ringLooks, seed3 ) =
            Rng.shuffle ringPalette seed2

        ( wandLooks, seed4 ) =
            Rng.shuffle wandPalette seed3
    in
    ( Dict.fromList
        (List.map2 Tuple.pair (idsOf isPotion) potionLooks
            ++ List.map2 Tuple.pair (idsOf isScroll) scrollLooks
            ++ List.map2 Tuple.pair (idsOf isRing) ringLooks
            ++ List.map2 Tuple.pair (idsOf isWand) wandLooks
        )
    , seed4
    )


lookAdjective : Idents -> ItemDef -> String
lookAdjective idents def =
    Dict.get def.id idents.looks |> Maybe.map .adjective |> Maybe.withDefault ""


{-| The name to show for an item: its true name once identified, else its per-run disguised appearance
("<adjective> potion", "scroll labeled <RUNE>", "<gem> ring", "<wood> wand"). -}
displayName : Idents -> ItemDef -> String
displayName idents def =
    let
        known =
            Set.member def.id idents.known
    in
    case def.kind of
        Content.Wand spec ->
            (if known then
                def.name

             else
                lookAdjective idents def ++ " wand"
            )
                ++ " ("
                ++ String.fromInt spec.charges
                ++ ")"

        Content.Artifact spec ->
            def.name
                ++ (if spec.charge >= spec.maxCharge then
                        " ✦"

                    else
                        " (" ++ String.fromInt spec.charge ++ "/" ++ String.fromInt spec.maxCharge ++ ")"
                   )

        Content.Equipment Content.RingSlot bonus ->
            if known then
                def.name ++ plusSuffix bonus

            else
                lookAdjective idents def ++ " ring"

        Content.Equipment _ bonus ->
            def.name ++ plusSuffix bonus

        _ ->
            if not (unidentifiable def) || known then
                def.name

            else if isScroll def then
                "scroll labeled " ++ lookAdjective idents def

            else
                lookAdjective idents def ++ " potion"


plusSuffix : Content.EquipBonus -> String
plusSuffix bonus =
    if bonus.plus > 0 then
        " +" ++ String.fromInt bonus.plus

    else
        ""


{-| The colour to draw an item with: true colour once identified, else its appearance colour. -}
displayColor : Idents -> ItemDef -> String
displayColor idents def =
    if not (unidentifiable def) || Set.member def.id idents.known then
        def.color

    else
        case Dict.get def.id idents.looks of
            Just look ->
                look.color

            Nothing ->
                def.color


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


damageHero : Int -> Game -> Game
damageHero dmg game =
    let
        hero =
            game.hero

        actual =
            if List.member "glass-cannon" game.challenges then
                dmg * 2

            else
                dmg

        -- A Shield status soaks damage before HP; the absorbed amount drains its magnitude.
        shieldLeft =
            hero.statuses
                |> List.filter (\s -> s.kind == Shielded)
                |> List.map .magnitude
                |> List.sum

        absorbed =
            min shieldLeft actual

        toHp =
            actual - absorbed

        drainedStatuses =
            List.filterMap
                (\s ->
                    if s.kind == Shielded then
                        let
                            left =
                                s.magnitude - absorbed
                        in
                        if left > 0 then
                            Just { s | magnitude = left }

                        else
                            Nothing

                    else
                        Just s
                )
                hero.statuses
    in
    { game | hero = { hero | hp = hero.hp - toHp, statuses = drainedStatuses } }


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
{-| Roll a slain monster's loot-table drop onto its cell: bosses always drop their relic, others ~30%. -}
dropLoot : Enemy -> Game -> Game
dropLoot enemy game =
    case enemy.def.drop |> Maybe.andThen (\itemId -> Content.findItem itemId game.ruleset) of
        Just def ->
            let
                ( roll, seed1 ) =
                    Rng.int 100 game.seed
            in
            if enemy.def.boss || roll < 30 then
                { game | items = { def = def, pos = enemy.pos } :: game.items, seed = seed1 }
                    |> addLog ("The " ++ enemy.def.name ++ " drops " ++ withArticle (displayName game.idents def) ++ "!")

            else
                { game | seed = seed1 }

        Nothing ->
            game


heroAttack : Enemy -> Game -> Game
heroAttack enemy game =
    let
        -- A foe that hasn't noticed you (unaware), is asleep, or that you strike from cover (you stand
        -- in tall grass) takes a surprise attack for doubled damage.
        surprised =
            not enemy.alerted
                || List.any (\s -> s.kind == Paralyzed) enemy.statuses
                || (Level.at game.hero.pos game.level == Grass)

        ( base, seed1 ) =
            rollDamage (heroDamage game.hero) enemy.def.defense game.seed

        glass =
            if List.member "glass-cannon" game.challenges then
                2

            else
                1

        raw =
            glass
                * (if surprised then
                    base
                        * (if game.hero.subclass == Just "Stalker" then
                            3

                           else
                            2
                          )

                   else
                    base
                  )

        -- A Vulnerable foe takes 50% more.
        dmg =
            if List.any (\s -> s.kind == Vulnerable) enemy.statuses then
                raw * 3 // 2

            else
                raw

        color =
            if surprised then
                "#ff7adf"

            else
                "#ffd166"

        remaining =
            enemy.hp - dmg

        surpriseNote =
            if surprised then
                " (surprise!)"

            else
                ""
    in
    if remaining <= 0 && enemy.revives > 0 then
        -- A ghoul shrugs off the killing blow and claws back up, one fewer life to spare.
        { game
            | enemies = updateEnemyAt enemy.pos (\e -> { e | hp = max 1 (e.def.maxHp // 2), revives = e.revives - 1, alerted = True }) game.enemies
            , seed = seed1
        }
            |> addLog ("The " ++ enemy.def.name ++ " collapses — then drags itself back up!")
            |> addPopup enemy.pos (String.fromInt dmg) color

    else if remaining <= 0 then
        { game
            | enemies = List.filter (\e -> e.pos /= enemy.pos) game.enemies
            , seed = seed1
            , kills = game.kills + 1
        }
            |> addLog ("You kill the " ++ enemy.def.name ++ "." ++ surpriseNote)
            |> addPopup enemy.pos (String.fromInt dmg) color
            |> gainXp enemy.def.xp
            |> dropLoot enemy
            |> applyHitEnchant enemy dmg

    else
        { game
            | enemies = updateEnemyAt enemy.pos (\e -> { e | hp = remaining, alerted = True }) game.enemies
            , seed = seed1
        }
            |> addLog ("You hit the " ++ enemy.def.name ++ " (" ++ String.fromInt dmg ++ ")" ++ surpriseNote ++ ".")
            |> addPopup enemy.pos (String.fromInt dmg) color
            |> applyHitEnchant enemy dmg
            |> maybeSplit enemy remaining


{-| On-hit weapon enchantment effects: **blazing** ignites the struck foe, **vampiric** heals the hero
for a third of the blow. -}
applyHitEnchant : Enemy -> Int -> Game -> Game
applyHitEnchant enemy dmg game =
    let
        hero =
            game.hero
    in
    case itemEnchant hero.weapon of
        "blazing" ->
            { game | enemies = updateEnemyAt enemy.pos (\e -> { e | statuses = addEnemyStatus Burn 2 3 e.statuses }) game.enemies }

        "grim" ->
            { game | enemies = updateEnemyAt enemy.pos (\e -> { e | statuses = addEnemyStatus Bleed 2 4 e.statuses }) game.enemies }

        "vampiric" ->
            let
                heal =
                    max 1 (dmg // 3)
            in
            { game | hero = { hero | hp = min hero.maxHp (hero.hp + heal) } }

        _ ->
            game


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
                        { def = parent.def, pos = spot, hp = childHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
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
            -- +6 HP each level, and an extra point of damage every fourth, to keep pace to depth 12.
            dmgGain =
                if modBy 4 (hero.level + 1) == 0 then
                    2

                else
                    1

            leveled =
                { hero
                    | xp = hero.xp - xpToNext hero.level
                    , level = hero.level + 1
                    , maxHp = hero.maxHp + 6
                    , damage = hero.damage + dmgGain
                    , hp = hero.maxHp + 6
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
                List.foldl stepEnemy ( [], { hero = game.hero, seed = game.seed, log = game.log, occupied = occupied0, level = game.level, glassCannon = List.member "glass-cannon" game.challenges, badderBosses = List.member "badder-bosses" game.challenges, foes = game.enemies |> List.filter (\e -> not e.ally) |> List.map .pos } ) game.enemies
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
    , glassCannon : Bool
    , badderBosses : Bool
    , foes : List Pos
    }


{-| The nearest non-ally foe position to an ally (for it to hunt), from the turn's foe snapshot. -}
nearestFoeOf : Enemy -> TurnAcc -> Maybe Pos
nearestFoeOf enemy acc =
    acc.foes
        |> List.foldl
            (\p best ->
                case best of
                    Nothing ->
                        Just p

                    Just b ->
                        if Grid.chebyshev p enemy.pos < Grid.chebyshev b enemy.pos then
                            Just p

                        else
                            best
            )
            Nothing


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

        heroHidden =
            hasStatus Invisible acc.hero

        los =
            Fov.visibleFrom healed.pos heroPos acc.level

        -- An invisible hero can't be acquired or tracked at range; only an adjacent foe still reacts.
        aware =
            if heroHidden then
                dist == 1

            else
                healed.alerted || (dist <= aggroRange && los)

        woken =
            { healed | alerted = healed.alerted && not heroHidden || aware }
    in
    if List.any (\s -> s.kind == Paralyzed) enemy.statuses then
        -- Asleep / paralysed: it idles this turn (its status counts down in tickEnemyStatuses).
        ( woken :: done, acc )

    else if enemy.ally then
        -- A corrupted ally hunts the nearest foe (the `allyCombat` pass deals the blows when adjacent).
        case nearestFoeOf enemy acc of
            Just foePos ->
                if Grid.chebyshev enemy.pos foePos <= 1 then
                    ( enemy :: done, acc )

                else
                    moveEnemy enemy enemy (Path.firstStep acc.level acc.occupied enemy.pos foePos) done acc

            Nothing ->
                moveEnemy enemy enemy (Path.firstStep acc.level acc.occupied enemy.pos heroPos) done acc

    else if not aware then
        ( woken :: done, acc )

    else
        let
            -- A boss or a necromancer may summon a minion before acting.
            ( done1, acc1 ) =
                if woken.def.boss || woken.def.ability == Content.SummonsAllies then
                    trySummon woken done acc

                else
                    ( done, acc )

            -- An aquatic monster (piranha) only advances through water; on land it lurks in place.
            stepCell =
                Path.firstStep acc1.level acc1.occupied enemy.pos heroPos

            landlocked =
                woken.def.ability
                    == Content.Aquatic
                    && (case stepCell of
                            Just p ->
                                Level.at p acc1.level /= Water

                            Nothing ->
                                True
                       )
        in
        if dist == 1 && woken.def.ranged > 0 && los then
            -- Kiting: a ranged attacker backs off to keep its distance, shooting from afar instead of
            -- being cornered in melee. If it can't retreat, it shoots point-blank.
            case stepAway enemy.pos heroPos acc1.level acc1.occupied of
                Just away ->
                    moveEnemy enemy woken (Just away) done1 acc1

                Nothing ->
                    attackHero woken (enemy.def.name ++ " shoots you") done1 acc1

        else if dist == 1 then
            case woken.def.ability of
                Content.StealsGold amount ->
                    stealAndFlee woken amount done1 acc1

                _ ->
                    attackHero woken (enemy.def.name ++ " hits you") done1 acc1

        else if isFleeing woken then
            moveEnemy enemy woken (stepAway enemy.pos heroPos acc1.level acc1.occupied) done1 acc1

        else if woken.def.ranged > 0 && dist <= woken.def.ranged && los then
            attackHero woken (enemy.def.name ++ " shoots you") done1 acc1

        else if landlocked then
            ( woken :: done1, acc1 )

        else
            moveEnemy enemy woken stepCell done1 acc1


{-| A boss occasionally summons a weak minion onto a free adjacent cell. -}
trySummon : Enemy -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
trySummon boss done acc =
    let
        ( roll, seed1 ) =
            Rng.int 100 acc.seed

        free =
            Grid.neighbors8 boss.pos
                |> List.filter (\p -> Level.isPassableAt p acc.level && not (Set.member ( p.x, p.y ) acc.occupied))
                |> List.head
    in
    case ( roll < 12, free ) of
        ( True, Just spot ) ->
            let
                def =
                    minionDef boss.def

                minion =
                    { def = def, pos = spot, hp = def.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
            in
            ( minion :: done
            , { acc
                | seed = seed1
                , occupied = Set.insert ( spot.x, spot.y ) acc.occupied
                , log = ("The " ++ boss.def.name ++ " summons a minion!") :: acc.log
              }
            )

        _ ->
            ( done, { acc | seed = seed1 } )


{-| A weakened spawn derived from a boss. -}
minionDef : EnemyDef -> EnemyDef
minionDef bossDef =
    { bossDef
        | name = bossDef.name ++ " spawn"
        , maxHp = max 4 (bossDef.maxHp // 6)
        , damage = max 1 (bossDef.damage // 2)
        , defense = 0
        , ranged = 0
        , ability = Content.NoAbility
        , boss = False
        , xp = 1
        , glyph = String.toLower bossDef.glyph
    }


{-| A `Regenerates` monster heals a little at the start of its turn. -}
applyRegen : Enemy -> Enemy
applyRegen enemy =
    case enemy.def.ability of
        Content.Regenerates n ->
            { enemy | hp = min enemy.def.maxHp (enemy.hp + n) }

        _ ->
            enemy


{-| Pack tactics: a roused monster the hero can see alerts its packmates within three cells, so groups
wake and close in together rather than one at a time. -}
packAlert : Game -> Game
packAlert game =
    let
        sources =
            game.enemies
                |> List.filter (\e -> e.alerted && not e.ally && Set.member ( e.pos.x, e.pos.y ) game.visible)
                |> List.map .pos
    in
    if List.isEmpty sources then
        game

    else
        { game
            | enemies =
                List.map
                    (\e ->
                        if not e.alerted && not e.ally && List.any (\p -> Grid.chebyshev p e.pos <= 3) sources then
                            { e | alerted = True }

                        else
                            e
                    )
                    game.enemies
        }


{-| Corrupted allies strike: each ally adjacent to a non-ally foe wounds it (killing if it drops),
clearing slain foes without awarding the hero XP. -}
allyCombat : Game -> Game
allyCombat game =
    let
        allies =
            List.filter (\e -> e.ally) game.enemies

        attackedPositions =
            allies
                |> List.filterMap
                    (\a ->
                        game.enemies
                            |> List.filter (\e -> not e.ally && Grid.chebyshev e.pos a.pos == 1)
                            |> List.head
                            |> Maybe.map (\foe -> ( foe.pos, a.def.damage ))
                    )

        damageAt pos dmg enemies =
            List.filterMap
                (\e ->
                    if e.pos == pos && not e.ally then
                        if e.hp - dmg <= 0 then
                            Nothing

                        else
                            Just { e | hp = e.hp - dmg, alerted = True }

                    else
                        Just e
                )
                enemies

        resolved =
            List.foldl (\( pos, dmg ) es -> damageAt pos dmg es) game.enemies attackedPositions
    in
    if List.isEmpty attackedPositions then
        game

    else
        { game | enemies = resolved }


{-| An enraged boss (below half HP) occasionally unleashes a signature blast of paralytic gas around
itself — a slam shockwave / web burst that makes its arena dangerous to stand near. -}
applyBossHazards : Game -> Game
applyBossHazards game =
    let
        enragedBoss =
            game.enemies
                |> List.filter (\e -> e.def.boss && e.alerted && e.hp * 2 < e.def.maxHp)
                |> List.head
    in
    case enragedBoss of
        Just boss ->
            let
                ( roll, seed1 ) =
                    Rng.int 100 game.seed
            in
            if roll < 18 then
                spawnGas ParalyticGasCloud 4 boss.pos { game | seed = seed1 }
                    |> addLog ("The " ++ boss.def.name ++ " unleashes a paralysing burst!")

            else
                { game | seed = seed1 }

        Nothing ->
            game


{-| Caster auras: a `Heals` monster (a shaman) mends every nearby ally (and itself) each turn. -}
applyEnemyAuras : Game -> Game
applyEnemyAuras game =
    let
        healers =
            game.enemies
                |> List.filterMap
                    (\e ->
                        case e.def.ability of
                            Content.Heals n ->
                                Just ( e.pos, n )

                            _ ->
                                Nothing
                    )
    in
    if List.isEmpty healers then
        game

    else
        let
            mend e =
                let
                    bonus =
                        healers
                            |> List.filter (\( hp, _ ) -> Grid.chebyshev hp e.pos <= 3)
                            |> List.map Tuple.second
                            |> List.sum
                in
                if bonus > 0 && e.hp < e.def.maxHp then
                    { e | hp = min e.def.maxHp (e.hp + bonus) }

                else
                    e
        in
        { game | enemies = List.map mend game.enemies }


{-| Fungal monsters periodically belch a small spore cloud at their feet — a creeping toxic hazard
that pressures the hero to finish them quickly rather than trade blows in the haze. -}
applySpores : Game -> Game
applySpores game =
    let
        sporers =
            game.enemies
                |> List.filter (\e -> e.def.ability == Content.Spores && not e.ally)
    in
    if List.isEmpty sporers || modBy 3 game.turn /= 0 then
        game

    else
        List.foldl (\e g -> spawnGas ToxicGasCloud 2 e.pos g) game sporers


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


{-| A monster flees when badly hurt or after stealing — but bosses never flee, they fight to the end. -}
isFleeing : Enemy -> Bool
isFleeing enemy =
    not enemy.def.boss && (enemy.fleeing || enemy.hp * 4 < enemy.def.maxHp)


attackHero : Enemy -> String -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
attackHero enemy verb done acc =
    let
        -- An enraged boss (below half HP, or always with the Badder Bosses challenge) hits harder.
        enrage =
            if enemy.def.boss && (acc.badderBosses || enemy.hp * 2 < enemy.def.maxHp) then
                3

            else
                0

        weaken =
            if List.any (\s -> s.kind == Weakened) enemy.statuses then
                2

            else
                0

        ( rolled, seed1 ) =
            rollDamage (max 1 (enemy.def.damage + enrage - weaken)) (heroDefense acc.hero) acc.seed

        glassed =
            if acc.glassCannon then
                rolled * 2

            else
                rolled

        dmg =
            if hasStatus Vulnerable acc.hero then
                glassed * 3 // 2

            else
                glassed

        hero =
            acc.hero

        -- A blazing champion sets the hero alight on a hit; a warlock's hex saps strength.
        ( burnt, burnLog ) =
            if enemy.def.ability == Content.Burns then
                ( addEnemyStatus Burn 3 3 hero.statuses, " You catch fire!" )

            else if enemy.def.ability == Content.Weakens then
                ( addEnemyStatus Weakened 1 5 hero.statuses, " A hex saps your strength!" )

            else if enemy.def.ability == Content.Charms then
                ( addEnemyStatus Charmed 1 4 hero.statuses, " You are charmed!" )

            else
                ( hero.statuses, "" )

        -- Thorns armour reflects a barb of damage back at the attacker.
        ( reflectedEnemy, thornLog ) =
            if itemEnchant hero.armour == "thorns" then
                ( { enemy | hp = enemy.hp - 3 }, " Thorns bite back!" )

            else
                ( enemy, "" )
    in
    ( reflectedEnemy :: done
    , { acc
        | hero = { hero | hp = hero.hp - dmg, statuses = burnt }
        , seed = seed1
        , log = ("The " ++ verb ++ " (" ++ String.fromInt dmg ++ ")." ++ burnLog ++ thornLog) :: acc.log
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
        case findIndex (\it -> it.id == "ankh") game.hero.inventory of
            Just idx ->
                -- An ankh shatters to pull the hero back from death, restoring half their health.
                let
                    hero =
                        game.hero
                in
                { game
                    | hero =
                        { hero
                            | inventory = removeAt idx hero.inventory
                            , hp = max 1 (hero.maxHp // 2)
                            , statuses = []
                        }
                }
                    |> addLog "Your ankh blazes and shatters — you are wrenched back from death!"

            Nothing ->
                { game | gameOver = True } |> addLog "You die. Press R to restart."

    else
        game


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex pred xs =
    let
        go i ys =
            case ys of
                [] ->
                    Nothing

                y :: rest ->
                    if pred y then
                        Just i

                    else
                        go (i + 1) rest
    in
    go 0 xs


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
        darkness =
            if List.member "darkness" game.challenges then
                2

            else
                0

        radius =
            max 2 (fovRadiusFor game.hero game.depth - darkness)

        vis =
            Fov.compute radius game.hero.pos game.level
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

            else if hasStatus Slowed bumped.hero || hasStatus Crippled bumped.hero then
                2

            else
                1

        afterEnemyDot =
            tickEnemyStatuses bumped

        afterAuras =
            applySpores (applyEnemyAuras afterEnemyDot)

        afterHazards =
            applyBossHazards afterAuras

        afterAllies =
            allyCombat afterHazards

        afterPack =
            packAlert afterAllies

        afterStatues =
            animateStatues afterPack

        afterMonsters =
            applyTimes enemyPhases enemiesTurn afterStatues

        afterGas =
            tickGas afterMonsters

        afterFire =
            tickFire afterGas

        afterIce =
            tickIce afterFire

        afterPerception =
            passivePerception afterIce

        afterHunger =
            tickHunger afterPerception

        recharged =
            rechargeAbility (rechargeWands { afterHunger | turn = afterHunger.turn + 1 })
    in
    checkQuest (maybeWander (ambientEvent (cursedBackfire (ringRegen recharged))))


{-| The Ring of Regeneration knits wounds over time: 1 HP back every few turns while it's worn. -}
ringRegen : Game -> Game
ringRegen game =
    let
        wearing =
            case game.hero.ring of
                Just r ->
                    r.id == "ring-regen"

                Nothing ->
                    False

        hero =
            game.hero
    in
    if wearing && hero.hp < hero.maxHp && modBy 5 game.turn == 0 then
        { game | hero = { hero | hp = min hero.maxHp (hero.hp + 1) } }

    else
        game


{-| Deep floors are unstable: an occasional ambient hazard (a quake raining rubble, or a vent of gas)
keeps the depths tense even between fights. Only fires from the Caves down (depth >= 6). -}
ambientEvent : Game -> Game
ambientEvent game =
    if game.depth < 6 || game.gameOver then
        game

    else
        let
            ( fires, seed1 ) =
                Rng.chance 3 game.seed
        in
        if not fires then
            { game | seed = seed1 }

        else
            let
                ( pick, seed2 ) =
                    Rng.int 2 seed1
            in
            if pick == 0 then
                let
                    ( dmg, seed3 ) =
                        Rng.range 2 (2 + game.depth // 3) seed2
                in
                checkHeroDeath
                    (damageHero dmg { game | seed = seed3 }
                        |> addLog ("The floor heaves — falling rubble strikes you! (" ++ String.fromInt dmg ++ ")")
                    )

            else
                spawnGas ToxicGasCloud 3 game.hero.pos { game | seed = seed2 }
                    |> addLog "A vent cracks open, hissing toxic gas into the air!"


{-| Cursed worn gear occasionally lashes out: a small chance each turn of a malign hiccup (a sting of
damage or a brief debuff). The more cursed pieces equipped, the likelier it bites. -}
cursedBackfire : Game -> Game
cursedBackfire game =
    let
        cursedCount =
            [ game.hero.weapon, game.hero.armour, game.hero.ring ]
                |> List.filterMap identity
                |> List.filter isCursed
                |> List.length
    in
    if cursedCount == 0 then
        game

    else
        let
            ( fires, seed1 ) =
                Rng.chance (4 * cursedCount) game.seed
        in
        if not fires then
            { game | seed = seed1 }

        else
            let
                ( which, seed2 ) =
                    Rng.int 3 seed1
            in
            case which of
                0 ->
                    checkHeroDeath (damageHero 2 { game | seed = seed2 } |> addLog "Your cursed gear bites into you!")

                1 ->
                    addStatus Weakened 1 4 { game | seed = seed2 } |> addLog "A curse saps your strength."

                _ ->
                    addStatus Slowed 1 3 { game | seed = seed2 } |> addLog "Cursed gear drags at your limbs."


{-| Build the hero's class-ability charge by one each turn, up to its maximum. -}
rechargeAbility : Game -> Game
rechargeAbility game =
    let
        hero =
            game.hero
    in
    if hero.abilityCharge < abilityMax then
        { game | hero = { hero | abilityCharge = hero.abilityCharge + 1 } }

    else
        game


{-| Deliver the imp's bounty once its kill target is reached. -}
checkQuest : Game -> Game
checkQuest game =
    case game.quest of
        Just quest ->
            if (quest.targetKills > 0 && game.kills >= quest.targetKills) || (quest.targetDepth > 0 && game.depth >= quest.targetDepth) then
                let
                    hero =
                        game.hero
                in
                { game | quest = Nothing, hero = { hero | inventory = hero.inventory ++ [ quest.reward ] } }
                    |> addLog ("The " ++ quest.giver ++ "'s bounty is fulfilled — " ++ withArticle (displayName game.idents quest.reward) ++ " appears in your pack!")

            else
                game

        Nothing ->
            game


{-| Recharge carried relics each turn: wands regain a charge every 12 turns; artifacts build one
charge per turn toward their maximum. -}
rechargeWands : Game -> Game
rechargeWands game =
    let
        hero =
            game.hero

        -- The Ring of Energy quickens every charge: wands tick up faster and artifacts gain double.
        energized =
            case hero.ring of
                Just r ->
                    r.id == "ring-power"

                Nothing ->
                    False

        wandTick =
            modBy
                (if energized then
                    8

                 else
                    12
                )
                game.turn
                == 0

        artifactGain =
            if energized then
                2

            else
                1

        bumped =
            List.map
                (\it ->
                    case it.kind of
                        Content.Wand spec ->
                            if wandTick && spec.charges < spec.maxCharges then
                                { it | kind = Content.Wand { spec | charges = spec.charges + 1 } }

                            else
                                it

                        Content.Artifact spec ->
                            if spec.charge < spec.maxCharge then
                                { it | kind = Content.Artifact { spec | charge = min spec.maxCharge (spec.charge + artifactGain) } }

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
    let
        -- The Amulet's bearer is hunted: waves come twice as fast, larger, and already awake.
        interval =
            if game.ascending then
                20

            else
                40

        cap =
            Content.spawnCountForDepth game.depth
                + (if game.ascending then
                    9

                   else
                    3
                  )
    in
    if game.gameOver || modBy interval game.turn /= 0 || List.length game.enemies >= cap then
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
                        | enemies = { def = def, pos = spot, hp = def.maxHp, alerted = game.ascending, fleeing = False, statuses = [], ally = False, revives = 0 } :: game.enemies
                        , seed = s2
                    }

            _ ->
                game


-- GAS CLOUDS -------------------------------------------------------------------------------------


{-| Stamp a gas cloud (and a one-cell border) onto the passable cells around `center`. -}
spawnGas : GasKind -> Int -> Pos -> Game -> Game
spawnGas kind density center game =
    let
        cells =
            (center :: Grid.neighbors4 center)
                |> List.filter (\p -> Level.isPassableAt p game.level)

        added =
            List.foldl (\p d -> Dict.insert ( p.x, p.y ) { kind = kind, density = density } d) game.gas cells
    in
    { game | gas = added }


{-| Diffuse every gas cloud one cell outward and thin it by one, then apply its effect to whoever
stands in it. Each non-wall cell takes the strongest of (its own density - 1) and (a neighbour's
density - 1); at zero it clears. -}
tickGas : Game -> Game
tickGas game =
    if Dict.isEmpty game.gas then
        game

    else
        let
            at key =
                Dict.get key game.gas

            candidateKeys =
                Dict.keys game.gas
                    |> List.concatMap
                        (\( x, y ) ->
                            ( x, y ) :: List.map (\p -> ( p.x, p.y )) (Grid.neighbors4 { x = x, y = y })
                        )
                    |> Set.fromList
                    |> Set.toList

            sourcesFor ( x, y ) =
                (( x, y ) :: List.map (\p -> ( p.x, p.y )) (Grid.neighbors4 { x = x, y = y }))
                    |> List.filterMap (\k -> at k |> Maybe.map (\g -> { kind = g.kind, density = g.density - 1 }))

            next ( x, y ) =
                if Level.isPassableAt { x = x, y = y } game.level then
                    case maximumBy .density (sourcesFor ( x, y )) of
                        Just best ->
                            if best.density > 0 then
                                Just ( ( x, y ), { kind = best.kind, density = best.density } )

                            else
                                Nothing

                        Nothing ->
                            Nothing

                else
                    Nothing

            newGas =
                List.filterMap next candidateKeys
        in
        applyGasEffects { game | gas = Dict.fromList newGas }


{-| Apply the gas under the hero (status) and under each monster (status) this turn. -}
applyGasEffects : Game -> Game
applyGasEffects game =
    let
        hero =
            game.hero

        afterHero =
            case Dict.get ( hero.pos.x, hero.pos.y ) game.gas of
                Just g ->
                    addStatus (gasStatus g.kind) (gasMagnitude g.kind) 2 game
                        |> addLog (gasLog g.kind)

                Nothing ->
                    game

        affected e =
            case Dict.get ( e.pos.x, e.pos.y ) afterHero.gas of
                Just g ->
                    { e | statuses = addEnemyStatus (gasStatus g.kind) (gasMagnitude g.kind) 2 e.statuses, alerted = True }

                Nothing ->
                    e
    in
    { afterHero | enemies = List.map affected afterHero.enemies }


gasStatus : GasKind -> StatusKind
gasStatus kind =
    case kind of
        ParalyticGasCloud ->
            Paralyzed

        _ ->
            Poison


gasMagnitude : GasKind -> Int
gasMagnitude kind =
    case kind of
        ToxicGasCloud ->
            2

        CausticGasCloud ->
            3

        ParalyticGasCloud ->
            1


gasLog : GasKind -> String
gasLog kind =
    case kind of
        ParalyticGasCloud ->
            "Paralytic gas seizes your muscles!"

        _ ->
            "Choking gas burns your lungs!"


gasColor : GasKind -> String
gasColor kind =
    case kind of
        ToxicGasCloud ->
            "#7fae5a"

        CausticGasCloud ->
            "#b6d24a"

        ParalyticGasCloud ->
            "#d6c24a"


-- FIRE -------------------------------------------------------------------------------------------


{-| A cell catches fire (floor or grass only — never water, walls or chasm). -}
flammable : Pos -> Game -> Bool
flammable p game =
    case Level.at p game.level of
        Floor ->
            True

        Grass ->
            True

        _ ->
            False


{-| Ignite `center` and the flammable cells around it. -}
spawnFire : Pos -> Game -> Game
spawnFire center game =
    let
        cells =
            (center :: Grid.neighbors4 center)
                |> List.filter (\p -> flammable p game)

        lit =
            List.foldl (\p d -> Dict.insert ( p.x, p.y ) 4 d) game.fire cells
    in
    { game | fire = lit }


{-| Advance every fire: it spreads into adjacent tall grass, burns whoever stands in it, dies down by
one each turn, and leaves scorched floor where grass burns away. -}
tickFire : Game -> Game
tickFire game =
    if Dict.isEmpty game.fire then
        game

    else
        let
            burning =
                Dict.keys game.fire

            -- Tall grass next to a flame catches.
            ignited =
                burning
                    |> List.concatMap
                        (\( x, y ) ->
                            Grid.neighbors4 { x = x, y = y }
                                |> List.filter (\nb -> Level.at nb game.level == Grass && not (Dict.member ( nb.x, nb.y ) game.fire))
                        )

            withIgnited =
                List.foldl (\nb d -> Dict.insert ( nb.x, nb.y ) 4 d) game.fire ignited

            stepped =
                Dict.toList withIgnited |> List.map (\( k, t ) -> ( k, t - 1 ))

            ( alive, expired ) =
                List.partition (\( _, t ) -> t > 0) stepped

            -- Grass that finished burning becomes scorched floor.
            scorched =
                List.foldl
                    (\( ( x, y ), _ ) lv ->
                        if Level.at { x = x, y = y } lv == Grass then
                            Level.set { x = x, y = y } Floor lv

                        else
                            lv
                    )
                    game.level
                    expired
        in
        applyFireEffects { game | fire = Dict.fromList alive, level = scorched }


{-| Freeze the Water cells within one tile of `center` into ice patches (a timed overlay), dousing any
fire there. Ice is walkable (water already is) but visibly frozen, thawing back over several turns. -}
freezeWaterNear : Pos -> Game -> Game
freezeWaterNear center game =
    let
        frozen =
            cellsWithin 1 center
                |> List.filter (\p -> Level.at p game.level == Water)
    in
    if List.isEmpty frozen then
        game

    else
        { game
            | ice = List.foldl (\p d -> Dict.insert ( p.x, p.y ) 12 d) game.ice frozen
            , fire = List.foldl (\p d -> Dict.remove ( p.x, p.y ) d) game.fire frozen
        }


{-| Count down ice patches each turn, thawing those that reach zero back to open water. -}
tickIce : Game -> Game
tickIce game =
    if Dict.isEmpty game.ice then
        game

    else
        { game
            | ice =
                game.ice
                    |> Dict.toList
                    |> List.filterMap
                        (\( k, t ) ->
                            if t - 1 > 0 then
                                Just ( k, t - 1 )

                            else
                                Nothing
                        )
                    |> Dict.fromList
        }


{-| Burn the hero and any monster standing in fire this turn. -}
applyFireEffects : Game -> Game
applyFireEffects game =
    let
        afterHero =
            if Dict.member ( game.hero.pos.x, game.hero.pos.y ) game.fire then
                addStatus Burn 3 3 game |> addLog "Flames lick at you!"

            else
                game

        affected e =
            if Dict.member ( e.pos.x, e.pos.y ) afterHero.fire then
                { e | statuses = addEnemyStatus Burn 3 3 e.statuses, alerted = True }

            else
                e
    in
    { afterHero | enemies = List.map affected afterHero.enemies }


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
                        |> List.filter (\s -> s.kind == Burn || s.kind == Poison || s.kind == Bleed)
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

        drain =
            if List.member "starvation" game.challenges then
                2

            else
                1

        fed =
            hero.nutrition - drain
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

                        Bleed ->
                            ( dhp - status.magnitude, ("You bleed (" ++ String.fromInt status.magnitude ++ ").") :: ls )

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


{-| Add a status, or refresh the duration of one already active of the same kind. Opposing elements
cancel: fire melts a chill (Crippled) and a chill quenches a fire (Burn). -}
addStatus : StatusKind -> Int -> Int -> Game -> Game
addStatus kind magnitude turns game =
    let
        hero =
            game.hero

        opposite =
            case kind of
                Burn ->
                    [ Crippled ]

                Crippled ->
                    [ Burn ]

                _ ->
                    []

        others =
            List.filter (\s -> s.kind /= kind && not (List.member s.kind opposite)) hero.statuses
    in
    { game | hero = { hero | statuses = { kind = kind, magnitude = magnitude, turns = turns } :: others } }


{-| Strip the hero's harmful statuses (poison, burn, bleed, weakness, etc.) — as a heal does. -}
cureDebuffs : Game -> Game
cureDebuffs game =
    let
        hero =
            game.hero

        harmful s =
            List.member s.kind [ Poison, Burn, Bleed, Weakened, Vulnerable, Crippled, Slowed ]
    in
    { game | hero = { hero | statuses = List.filter (\s -> not (harmful s)) hero.statuses } }


nth : Int -> List a -> Maybe a
nth i xs =
    if i < 0 then
        Nothing

    else
        List.head (List.drop i xs)


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs


replaceAt : Int -> a -> List a -> List a
replaceAt i x xs =
    List.indexedMap
        (\j old ->
            if j == i then
                x

            else
                old
        )
        xs


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
            ++ plantGlyphs game.plants
            ++ wellGlyphs game.well
            ++ statueGlyphs game.statues
            ++ altarGlyphs game.altar
            ++ npcGlyphs game.npc
            ++ List.map chestGlyph game.chests
            ++ List.map shopGlyph game.shop
            ++ List.map (itemGlyph game.idents) game.items
            ++ List.map enemyGlyph game.enemies
            ++ [ heroGlyph game ]
    , popups = List.map (\p -> { pos = p.pos, text = p.text, color = p.color }) game.popups
    , gas =
        (game.gas
            |> Dict.toList
            |> List.filterMap
                (\( ( x, y ), g ) ->
                    if Set.member ( x, y ) game.visible then
                        Just { pos = { x = x, y = y }, color = gasColor g.kind, alpha = min 0.6 (0.12 + toFloat g.density * 0.08) }

                    else
                        Nothing
                )
        )
            ++ (game.fire
                    |> Dict.toList
                    |> List.filterMap
                        (\( ( x, y ), t ) ->
                            if Set.member ( x, y ) game.visible then
                                Just { pos = { x = x, y = y }, color = "#ff6a2a", alpha = min 0.7 (0.3 + toFloat t * 0.1) }

                            else
                                Nothing
                        )
               )
            ++ (game.ice
                    |> Dict.toList
                    |> List.filterMap
                        (\( ( x, y ), _ ) ->
                            if Set.member ( x, y ) game.visible then
                                Just { pos = { x = x, y = y }, color = "#bfe6ff", alpha = 0.34 }

                            else
                                Nothing
                        )
               )
    , healthBars =
        game.enemies
            |> List.filterMap
                (\e ->
                    if Set.member ( e.pos.x, e.pos.y ) game.visible && e.hp < e.def.maxHp then
                        Just { pos = e.pos, frac = toFloat (max 0 e.hp) / toFloat e.def.maxHp, ally = e.ally }

                    else
                        Nothing
                )
    , statusMarks =
        let
            marksFor pos statuses =
                case List.map (\s -> statusColor s.kind) statuses |> List.filter (\c -> c /= "") of
                    [] ->
                        Nothing

                    colors ->
                        Just { pos = pos, colors = List.take 4 colors }
        in
        (case marksFor game.hero.pos game.hero.statuses of
            Just m ->
                [ m ]

            Nothing ->
                []
        )
            ++ (game.enemies
                    |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
                    |> List.filterMap (\e -> marksFor e.pos e.statuses)
               )
    , mapMarkers =
        let
            explored p =
                Set.member ( p.x, p.y ) game.explored

            stairMarkers =
                (if explored game.stairsDown then
                    [ { pos = game.stairsDown, color = "#d8b24c" } ]

                 else
                    []
                )
                    ++ (if explored game.stairsUp then
                            [ { pos = game.stairsUp, color = "#4f8bff" } ]

                        else
                            []
                       )

            itemMarkers =
                game.items
                    |> List.filter (\it -> explored it.pos)
                    |> List.map (\it -> { pos = it.pos, color = "#ffe08a" })

            npcMarker =
                case game.npc of
                    Just n ->
                        if explored n.pos then
                            [ { pos = n.pos, color = "#6ad8c0" } ]

                        else
                            []

                    Nothing ->
                        []
        in
        stairMarkers ++ itemMarkers ++ npcMarker
    , theme = Render.themeForDepth game.depth
    , camera = game.hero.pos
    , cursor = Nothing
    , shake = False
    , pixelArt = True
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
        , ability =
            if game.hero.heroClass == "" then
                ""

            else if game.hero.abilityCharge >= abilityMax then
                abilityName game.hero.heroClass ++ " — READY (Q)"

            else
                abilityName game.hero.heroClass ++ " (" ++ String.fromInt game.hero.abilityCharge ++ "/" ++ String.fromInt abilityMax ++ ")"
        , statuses = List.map statusLabel game.hero.statuses
        , keyring = keyringLabel game.hero.inventory
        , boss =
            game.enemies
                |> List.filter (\e -> e.def.boss && not e.ally)
                |> List.head
                |> Maybe.map (\e -> e.def.name)
                |> Maybe.withDefault ""
        , score =
            max 0
                ((game.depth * 120)
                    + (game.kills * 15)
                    + (if game.won then
                        1500

                       else
                        0
                      )
                    - (game.turn // 20)
                )
        , inventory = List.map (displayName game.idents) (List.filter (\d -> not (isKey d)) game.hero.inventory)
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

    else if game.ascending then
        "ASCENDING — flee to the surface! (climb up-stairs with >)"

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
    , sprite =
        if game.hero.heroClass == "" then
            "hero"

        else
            "hero-" ++ game.hero.heroClass
    , tint = ""
    }


enemyGlyph : Enemy -> Render.Glyph
enemyGlyph enemy =
    { pos = enemy.pos
    , char = enemy.def.glyph
    , color =
        if enemy.ally then
            "#5dd47a"

        else
            enemy.def.color
    , layer = Render.layerActor
    , heavy = enemy.ally
    , sprite = enemy.def.id
    , tint =
        if enemy.ally then
            "#3ad17a"

        else
            ""
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
    , sprite = item.def.id
    , tint = ""
    }


npcGlyphs : Maybe Npc -> List Render.Glyph
npcGlyphs maybeNpc =
    case maybeNpc of
        Just n ->
            let
                ( ch, color ) =
                    case n.kind of
                        Ghost ->
                            ( "&", "#a9d6ff" )

                        Sage ->
                            ( "&", "#d6c27a" )

                        Wandmaker ->
                            ( "&", "#9be0ff" )

                        Blacksmith ->
                            ( "&", "#e0884b" )

                        Imp ->
                            ( "&", "#c97fe0" )
            in
            [ { pos = n.pos, char = ch, color = color, layer = Render.layerActor, heavy = True, sprite = "npc", tint = "" } ]

        Nothing ->
            []


statueGlyphs : List Pos -> List Render.Glyph
statueGlyphs statues =
    List.map
        (\p -> { pos = p, char = "&", color = "#7a7a82", layer = Render.layerItem, heavy = False, sprite = "statue", tint = "" })
        statues


wellGlyphs : Maybe Well -> List Render.Glyph
wellGlyphs maybeWell =
    case maybeWell of
        Just w ->
            let
                color =
                    case w.kind of
                        HealthWell ->
                            "#5dd47a"

                        AwarenessWell ->
                            "#82aaff"

                        TransmuteWell ->
                            "#6ad8c0"
            in
            [ { pos = w.pos, char = "○", color = color, layer = Render.layerItem, heavy = False, sprite = "well", tint = "" } ]

        Nothing ->
            []


plantGlyphs : Dict ( Int, Int ) PlantKind -> List Render.Glyph
plantGlyphs plants =
    plants
        |> Dict.toList
        |> List.map
            (\( ( x, y ), kind ) ->
                let
                    color =
                        case kind of
                            Firebloom ->
                                "#ff7a3c"

                            Sungrass ->
                                "#e0d24b"

                            Sorrowmoss ->
                                "#9b6ad8"

                            Earthroot ->
                                "#8a6a4a"
                in
                { pos = { x = x, y = y }, char = "♣", color = color, layer = Render.layerItem, heavy = False, sprite = "plant", tint = "" }
            )


chestGlyph : Chest -> Render.Glyph
chestGlyph chest =
    { pos = chest.pos
    , char = "0"
    , color =
        if chest.crystal then
            "#7fe0e0"

        else
            "#caa24a"
    , layer = Render.layerItem
    , heavy = True
    , sprite = "chest"
    , tint =
        if chest.crystal then
            "#7fe0e0"

        else
            ""
    }


altarGlyphs : Maybe Pos -> List Render.Glyph
altarGlyphs maybeAltar =
    case maybeAltar of
        Just p ->
            [ { pos = p, char = "_", color = "#9be0ff", layer = Render.layerItem, heavy = False, sprite = "altar", tint = "" } ]

        Nothing ->
            []


shopGlyph : ShopEntry -> Render.Glyph
shopGlyph entry =
    { pos = entry.pos
    , char = entry.def.glyph
    , color = "#ffd166"
    , layer = Render.layerItem
    , heavy = False
    , sprite = entry.def.id
    , tint = ""
    }


trapGlyph : Trap -> Render.Glyph
trapGlyph trap =
    { pos = trap.pos
    , char = "^"
    , color = "#e0824b"
    , layer = Render.layerTerrain
    , heavy = False
    , sprite = "trap"
    , tint = ""
    }
