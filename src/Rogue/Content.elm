module Rogue.Content exposing
    ( EnemyDef
    , Ruleset
    , HeroDef
    , enemiesForDepth
    , spawnCountForDepth
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
    }


{-| The enemy archetypes eligible to spawn at a given depth, paired with their spawn weight (for
`Rogue.Rng.pickWeighted`). An empty result means "spawn nothing on this floor". -}
enemiesForDepth : Int -> Ruleset -> List ( Int, EnemyDef )
enemiesForDepth depth ruleset =
    ruleset.enemies
        |> List.filter (\e -> depth >= e.minDepth && depth <= e.maxDepth)
        |> List.map (\e -> ( e.spawnWeight, e ))


{-| How many monsters to seed on a floor of the given depth — a gentle ramp. A mod that wants a
denser or sparser dungeon can scale this by overriding generation config later; for now it is a
shared default so all rulesets get a comparable population curve. -}
spawnCountForDepth : Int -> Int
spawnCountForDepth depth =
    min 12 (3 + depth)
