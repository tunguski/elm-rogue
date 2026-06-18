module Rogue.Game.Types exposing (..)

{-| The engine's shared data types and the small, pure helpers that read them (status labels,
the hero's effective damage/defense, FOV radius, a few floor constants). Split out of
`Rogue.Game` so the feature modules (combat, items, the monster turn, scene projection, …) can
all share one definition of `Game`/`Hero`/`Enemy` without importing each other in a cycle.

`Rogue.Game` re-exposes the public types from here, so importers still see `Game.Game` etc. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, Ruleset)
import Rogue.Dungeon exposing (Room)
import Rogue.Grid as Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Rng exposing (Seed)
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
                + (if List.member "Executioner" hero.talents then
                    4

                   else
                    0
                  )
                + (if List.member "Bloodlust" hero.talents && hero.hp * 2 <= hero.maxHp then
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
                + (if List.member "Last Stand" hero.talents && hero.hp * 2 <= hero.maxHp then
                    4

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
