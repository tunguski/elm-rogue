module Mod.Default exposing (ruleset)

{-| The default mod: the stock bestiary and hero, expressed entirely as data.

This module is the reference example of how content is authored. To make a mod you copy this file,
tweak the numbers (or add/remove `EnemyDef`s) and hand the resulting `Ruleset` to the engine — see
`Mod.Hard` for a difficulty mod that does exactly that. The bestiary is loosely modelled on Shattered
Pixel Dungeon's early floors (rats and gnolls up top, tougher crabs and skeletons deeper).
-}

import Rogue.Content exposing (EnemyDef, HeroDef, Ruleset)


ruleset : Ruleset
ruleset =
    { name = "Default"
    , hero = hero
    , enemies =
        [ rat
        , marsupialRat
        , gnollScout
        , crab
        , skeleton
        , swarm
        ]
    }


hero : HeroDef
hero =
    { maxHp = 20
    , damage = 4
    , defense = 1
    , glyph = "@"
    , color = "#ffe08a"
    , fovRadius = 7
    }


rat : EnemyDef
rat =
    { id = "rat"
    , name = "rat"
    , glyph = "r"
    , color = "#b08968"
    , maxHp = 8
    , damage = 2
    , defense = 0
    , speed = 1
    , minDepth = 1
    , maxDepth = 3
    , spawnWeight = 10
    , xp = 1
    }


marsupialRat : EnemyDef
marsupialRat =
    { id = "marsupial-rat"
    , name = "marsupial rat"
    , glyph = "r"
    , color = "#caa472"
    , maxHp = 10
    , damage = 3
    , defense = 0
    , speed = 1
    , minDepth = 2
    , maxDepth = 4
    , spawnWeight = 6
    , xp = 2
    }


gnollScout : EnemyDef
gnollScout =
    { id = "gnoll-scout"
    , name = "gnoll scout"
    , glyph = "g"
    , color = "#7fae5a"
    , maxHp = 12
    , damage = 4
    , defense = 1
    , speed = 1
    , minDepth = 2
    , maxDepth = 5
    , spawnWeight = 8
    , xp = 3
    }


crab : EnemyDef
crab =
    { id = "crab"
    , name = "sewer crab"
    , glyph = "c"
    , color = "#d4756a"
    , maxHp = 15
    , damage = 5
    , defense = 2
    , speed = 1
    , minDepth = 3
    , maxDepth = 6
    , spawnWeight = 6
    , xp = 4
    }


skeleton : EnemyDef
skeleton =
    { id = "skeleton"
    , name = "skeleton"
    , glyph = "s"
    , color = "#d9d2c2"
    , maxHp = 18
    , damage = 6
    , defense = 2
    , speed = 1
    , minDepth = 4
    , maxDepth = 8
    , spawnWeight = 5
    , xp = 6
    }


swarm : EnemyDef
swarm =
    { id = "swarm"
    , name = "swarm of flies"
    , glyph = "w"
    , color = "#9aa7ba"
    , maxHp = 6
    , damage = 2
    , defense = 0
    , speed = 1
    , minDepth = 3
    , maxDepth = 7
    , spawnWeight = 4
    , xp = 2
    }
