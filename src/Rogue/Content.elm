module Rogue.Content exposing
    ( EnemyDef
    , MonsterAbility(..)
    , ItemDef
    , ItemEffect(..)
    , ItemKind(..)
    , WandSpec
    , ArtifactSpec
    , EquipSlot(..)
    , EquipBonus
    , ClassDef
    , Ruleset
    , HeroDef
    , enemiesForDepth
    , bossForDepth
    , itemsForDepth
    , spawnCountForDepth
    , itemCountForDepth
    , findItem
    , defaultClass
    , describe
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


{-| A special behaviour a monster can have, applied by the engine each turn / on hit. -}
type MonsterAbility
    = NoAbility
    | Regenerates Int
    | Splits
    | StealsGold Int
    | Burns
    | Heals Int
    | SummonsAllies
    | Aquatic


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
    , ranged : Int
    , ability : MonsterAbility
    , boss : Bool
    , minDepth : Int
    , maxDepth : Int
    , spawnWeight : Int
    , xp : Int
    , drop : Maybe String
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
    | Regenerate Int Int
    | TeleportSelf
    | MagicMap
    | IdentifyAll
    | UpgradeGear
    | Feed Int
    | LightFor Int Int
    | HasteFor Int
    | Recharge
    | Terror
    | RemoveCurse
    | Invisibility Int
    | Levitation Int
    | MindVision
    | Experience Int
    | Incinerate Int
    | ToxicGas Int
    | Lullaby Int
    | Retribution Int
    | Transmute
    | GrowGrass
    | Aggravate
    | Explode Int


{-| Which body slot a piece of equipment occupies. -}
type EquipSlot
    = WeaponSlot
    | ArmourSlot
    | RingSlot


{-| The passive stat bonuses a piece of equipment confers while worn. -}
type alias EquipBonus =
    { damage : Int
    , defense : Int
    , maxHp : Int
    , plus : Int
    , cursed : Bool
    , enchant : String
    }


{-| What kind of item this is. A `Consumable` is used up for an instant `ItemEffect`; `Equipment` is
worn in a slot for a passive `EquipBonus` until swapped out. Adding a new kind is an engine change
(a case in `Rogue.Game`); adding new *items* of an existing kind is pure data. -}
type ItemKind
    = Consumable ItemEffect
    | Equipment EquipSlot EquipBonus
    | Wand WandSpec
    | Artifact ArtifactSpec
    | Key


{-| An artifact: a reusable relic that charges up over time and, when full, unleashes an `ItemEffect`
(then resets to empty). Unlike a wand it isn't aimed — it acts on the hero / the floor. -}
type alias ArtifactSpec =
    { charge : Int
    , maxCharge : Int
    , effect : ItemEffect
    }


{-| A wand: zaps the nearest visible monster for `damage`. Each zap spends a charge; the wand slowly
recharges over time up to `maxCharges`, so it's never permanently spent. -}
type alias WandSpec =
    { damage : Int
    , charges : Int
    , maxCharges : Int
    , burns : Bool
    }


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


{-| A playable character class — a starting profile the player picks before a run. It overrides the
hero's stats and seeds opening gear (referenced by item `id` so a class never duplicates an
`ItemDef`). Classes live in the `Ruleset`, so a mod ships its own roster. -}
type alias ClassDef =
    { id : String
    , name : String
    , description : String
    , glyph : String
    , color : String
    , maxHp : Int
    , damage : Int
    , defense : Int
    , fovRadius : Int
    , startingWeapon : Maybe String
    , startingArmour : Maybe String
    , startingItems : List String
    }


{-| Everything a mod provides. Grows over milestones (generation config…); the engine only ever
reads it. -}
type alias Ruleset =
    { name : String
    , hero : HeroDef
    , classes : List ClassDef
    , enemies : List EnemyDef
    , bosses : List EnemyDef
    , items : List ItemDef
    }


{-| The enemy archetypes eligible to spawn at a given depth, paired with their spawn weight (for
`Rogue.Rng.pickWeighted`). An empty result means "spawn nothing on this floor". -}
enemiesForDepth : Int -> Ruleset -> List ( Int, EnemyDef )
enemiesForDepth depth ruleset =
    ruleset.enemies
        |> List.filter (\e -> depth >= e.minDepth && depth <= e.maxDepth)
        |> List.map (\e -> ( e.spawnWeight, e ))


{-| The boss guarding this depth, if any (its depth band must contain `depth`). -}
bossForDepth : Int -> Ruleset -> Maybe EnemyDef
bossForDepth depth ruleset =
    findHelp (\b -> depth >= b.minDepth && depth <= b.maxDepth) ruleset.bosses


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


{-| A one-line human description of what an item does, for the inventory screen. -}
enchantSuffix : String -> String
enchantSuffix enchant =
    if enchant == "" then
        ""

    else
        " · " ++ enchant


describe : ItemDef -> String
describe def =
    case def.kind of
        Equipment WeaponSlot bonus ->
            "weapon · +" ++ String.fromInt (bonus.damage + bonus.plus) ++ " damage" ++ enchantSuffix bonus.enchant

        Equipment ArmourSlot bonus ->
            "armour · +" ++ String.fromInt (bonus.defense + bonus.plus) ++ " defense" ++ enchantSuffix bonus.enchant

        Equipment RingSlot bonus ->
            "ring · +" ++ String.fromInt bonus.damage ++ " dmg / +" ++ String.fromInt bonus.defense ++ " def"

        Wand spec ->
            "wand · " ++ String.fromInt spec.damage ++ " dmg, " ++ String.fromInt spec.charges ++ " charges"

        Artifact spec ->
            "artifact · " ++ describeEffect spec.effect

        Key ->
            "opens a locked door"

        Consumable effect ->
            describeEffect effect


describeEffect : ItemEffect -> String
describeEffect effect =
    case effect of
        HealHp n ->
            "restores " ++ String.fromInt n ++ " HP"

        HealFull ->
            "restores all HP"

        MaxHpBonus n ->
            "+" ++ String.fromInt n ++ " max HP"

        DamageBonus n ->
            "+" ++ String.fromInt n ++ " damage"

        DefenseBonus n ->
            "+" ++ String.fromInt n ++ " defense"

        Gold n ->
            String.fromInt n ++ " gold"

        Regenerate perTurn turns ->
            "regenerate " ++ String.fromInt perTurn ++ "/turn for " ++ String.fromInt turns

        TeleportSelf ->
            "teleports you"

        MagicMap ->
            "reveals the floor"

        IdentifyAll ->
            "identifies potions"

        UpgradeGear ->
            "upgrades equipment"

        Feed n ->
            "food · +" ++ String.fromInt n ++ " nutrition"

        LightFor radius turns ->
            "light +" ++ String.fromInt radius ++ " for " ++ String.fromInt turns

        HasteFor turns ->
            "haste for " ++ String.fromInt turns

        Recharge ->
            "recharges your wands"

        Terror ->
            "routs nearby monsters"

        RemoveCurse ->
            "lifts curses from your gear"

        Invisibility turns ->
            "turns you invisible for " ++ String.fromInt turns

        Levitation turns ->
            "lets you levitate for " ++ String.fromInt turns

        MindVision ->
            "reveals nearby minds"

        Experience xp ->
            "grants " ++ String.fromInt xp ++ " experience"

        Incinerate _ ->
            "bursts into flame"

        ToxicGas _ ->
            "releases toxic gas"

        Lullaby _ ->
            "lulls nearby monsters to sleep"

        Retribution _ ->
            "blasts visible monsters"

        Transmute ->
            "transmutes a carried item"

        GrowGrass ->
            "grows sheltering grass"

        Aggravate ->
            "enrages the whole floor"

        Explode dmg ->
            "thrown: explodes for " ++ String.fromInt dmg


{-| Look up an item archetype by `id` (e.g. to resolve a class's starting gear). -}
findItem : String -> Ruleset -> Maybe ItemDef
findItem id ruleset =
    findHelp (\i -> i.id == id) ruleset.items


{-| A fallback class synthesised from the ruleset's `hero` stats — used when a ruleset defines no
classes, so the engine always has something to start with. -}
defaultClass : Ruleset -> ClassDef
defaultClass ruleset =
    case ruleset.classes of
        first :: _ ->
            first

        [] ->
            { id = "adventurer"
            , name = "Adventurer"
            , description = "A plain delver."
            , glyph = ruleset.hero.glyph
            , color = ruleset.hero.color
            , maxHp = ruleset.hero.maxHp
            , damage = ruleset.hero.damage
            , defense = ruleset.hero.defense
            , fovRadius = ruleset.hero.fovRadius
            , startingWeapon = Nothing
            , startingArmour = Nothing
            , startingItems = []
            }


findHelp : (a -> Bool) -> List a -> Maybe a
findHelp pred xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if pred x then
                Just x

            else
                findHelp pred rest
