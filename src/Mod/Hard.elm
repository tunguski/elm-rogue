module Mod.Hard exposing (ruleset)

{-| A difficulty mod, defined as a *transform* of `Mod.Default` rather than from scratch — the
clearest demonstration that "custom strengths of enemies and weapons" is just data.

Every monster hits harder and soaks more, wakes from further away is left to the engine, and healing
is scarcer. Because a `Ruleset` is plain data, a mod can `map` over another mod's roster instead of
re-authoring it; swap this for `Mod.Default` at the call site and the *same engine* plays a brutal
variant. This is the moddability contract end to end.
-}

import Mod.Default as Default
import Rogue.Content exposing (EnemyDef, ItemDef, Ruleset)


ruleset : Ruleset
ruleset =
    let
        base =
            Default.ruleset

        baseHero =
            base.hero
    in
    { base
        | name = "Hardcore"
        , hero = { baseHero | maxHp = 16 }
        , enemies = List.map toughen base.enemies
        , items = List.map scarcer base.items
    }


{-| +50% HP and damage, +1 defense, a wider depth band so each monster lingers a floor longer. -}
toughen : EnemyDef -> EnemyDef
toughen e =
    { e
        | maxHp = (e.maxHp * 3) // 2
        , damage = (e.damage * 3 + 1) // 2
        , defense = e.defense + 1
        , maxDepth = e.maxDepth + 1
        , xp = e.xp + 1
    }


{-| Healing and gold are rarer; permanent buffs keep their weight so progress is possible but tight. -}
scarcer : ItemDef -> ItemDef
scarcer i =
    if String.startsWith "potion-healing" i.id || String.startsWith "potion-greater" i.id || i.id == "gold" then
        { i | spawnWeight = max 1 (i.spawnWeight // 2) }

    else
        i
