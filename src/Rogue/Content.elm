module Rogue.Content exposing
    ( EnemyDef
    , ItemDef
    , ItemEffect(..)
    , ItemKind(..)
    , EquipSlot(..)
    , EquipBonus
    , Ruleset
    , HeroDef
    , enemiesForDepth
    , itemsForDepth
    , spawnCountForDepth
    , itemCountForDepth
    )

{-| The moddable content model — *data* that defines what fills a dungeon, kept separate from the
engine that runs it.

A `Ruleset` is a record a mod supplies wholesale: the hero's starting stats, the roster of
`EnemyDef`s (each with its own HP, damage, defense, depth band and spawn weight), and — in later
milestones — items and generation tuning. Because the engine takes the `Ruleset` as a parameter,
"custom strengths of enemies", a harder difficulty, or an entirely new bestiary are all just a
different value of this type. No engine code changes; nothing is hard-coded.

`Rogue.Content` deliberately holds only the *types* and pure selection helpers. Concrete content
lives in `Mod.Default` (and any other mod module), so the dependency only ever points content → here,
never the other way.
-}


{-| A monster archetype. Instances in play (`Rogue.Game.Enemy`) carry a copy of their `EnemyDef`, so
the stats a mod sets here are exactly the stats that fight. -}
type alias EnemyDef =
    { id : String
    , name : String
    , glyph : String
    , color : String
    , maxHp : Int
    , damage : Int
    , defense : Int
    , speed : Int
    , minDepth : Int
    , maxDepth : Int
    , spawnWeight : Int
    , xp : Int
    }


{-| What an item does when used, expressed as data the engine interprets. This closed vocabulary is
the seam mods build items from: a new *item* is a new `ItemDef` combining existing effects with its
own name/value; a genuinely new *kind* of effect is the one thing that needs an engine change (a new
constructor here plus its case in `Rogue.Game.applyEffect`). -}
type ItemEffect
    = HealHp Int
    | HealFull
    | MaxHpBonus Int
    | DamageBonus Int
    | DefenseBonus Int
    | Gold Int


{-| Which body slot a piece of equipment occupies. -}
type EquipSlot
    = WeaponSlot
    | ArmourSlot


{-| The passive stat bonuses a piece of equipment confers while worn. -}
type alias EquipBonus =
    { damage : Int
    , defense : Int
    , maxHp : Int
    }


{-| What kind of item this is. A `Consumable` is used up for an instant `ItemEffect`; `Equipment` is
worn in a slot for a passive `EquipBonus` until swapped out. Adding a new kind is an engine change
(a case in `Rogue.Game`); adding new *items* of an existing kind is pure data. -}
type ItemKind
    = Consumable ItemEffect
    | Equipment EquipSlot EquipBonus


{-| A pickup archetype — defined as data, exactly like `EnemyDef`. Instances on the floor carry a
copy, so the stats a mod sets here are what the player actually gets. -}
type alias ItemDef =
    { id : String
    , name : String
    , glyph : String
    , color : String
    , kind : ItemKind
    , minDepth : Int
    , maxDepth : Int
    , spawnWeight : Int
    }


{-| The hero's starting profile (also moddable). -}
type alias HeroDef =
    { maxHp : Int
    , damage : Int
    , defense : Int
    , glyph : String
    , color : String
    , fovRadius : Int
    }


{-| Everything a mod provides. Grows over milestones (items, generation config…); the engine only
ever reads it. -}
type alias Ruleset =
    { name : String
    , hero : HeroDef
    , enemies : List EnemyDef
    , items : List ItemDef
    }


{-| The enemy archetypes eligible to spawn at a given depth, paired with their spawn weight (for
`Rogue.Rng.pickWeighted`). An empty result means "spawn nothing on this floor". -}
enemiesForDepth : Int -> Ruleset -> List ( Int, EnemyDef )
enemiesForDepth depth ruleset =
    ruleset.enemies
        |> List.filter (\e -> depth >= e.minDepth && depth <= e.maxDepth)
        |> List.map (\e -> ( e.spawnWeight, e ))


{-| The item archetypes eligible at a given depth, paired with spawn weight (for `pickWeighted`). -}
itemsForDepth : Int -> Ruleset -> List ( Int, ItemDef )
itemsForDepth depth ruleset =
    ruleset.items
        |> List.filter (\i -> depth >= i.minDepth && depth <= i.maxDepth)
        |> List.map (\i -> ( i.spawnWeight, i ))


{-| How many monsters to seed on a floor of the given depth — a gentle ramp. A mod that wants a
denser or sparser dungeon can scale this by overriding generation config later; for now it is a
shared default so all rulesets get a comparable population curve. -}
spawnCountForDepth : Int -> Int
spawnCountForDepth depth =
    min 12 (3 + depth)


{-| How many floor items to seed on a floor of the given depth. -}
itemCountForDepth : Int -> Int
itemCountForDepth depth =
    min 6 (1 + depth // 2)
