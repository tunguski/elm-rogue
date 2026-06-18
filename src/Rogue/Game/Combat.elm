module Rogue.Game.Combat exposing (..)

{-| Combat resolution: the hero's and monsters' attacks, damage to the hero, loot drops on a
kill, and the experience/level-up rewards. The lowest of the turn-resolution layers. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Path as Path
import Rogue.Rng as Rng exposing (Seed)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)
import Rogue.Game.Types exposing (..)
import Rogue.Game.Appearance exposing (..)


-- COMBAT -----------------------------------------------------------------------------------------


{-| Damage rolls over a range — from roughly half the attack up to the full attack — then subtracts
the defender's defense, never below 1 (Shattered Pixel's swingy weapon damage). -}
rollDamage : Int -> Int -> Seed -> ( Int, Seed )
rollDamage attack defense seed =
    Rng.range (max 1 (attack // 2 - defense)) (max 1 (attack - defense)) seed


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
            if enemy.def.boss then
                bossBonusLoot enemy
                    { game | items = { def = def, pos = enemy.pos } :: game.items, seed = seed1 }
                    |> addLog ("The " ++ enemy.def.name ++ " drops " ++ withArticle (displayName game.idents def) ++ "!")

            else if roll < 30 then
                { game | items = { def = def, pos = enemy.pos } :: game.items, seed = seed1 }
                    |> addLog ("The " ++ enemy.def.name ++ " drops " ++ withArticle (displayName game.idents def) ++ "!")

            else
                { game | seed = seed1 }

        Nothing ->
            -- Even bosses with no relic in their def still pay out a victor's reward.
            if enemy.def.boss then
                bossBonusLoot enemy game

            else
                game


{-| A boss's guaranteed victor's reward beyond its relic: a scroll of upgrade scattered nearby plus a
purse of gold, so felling a boss always feels worth the fight. -}
bossBonusLoot : Enemy -> Game -> Game
bossBonusLoot enemy game =
    let
        ( bonusGold, seed1 ) =
            Rng.range (20 + game.depth * 5) (40 + game.depth * 8) game.seed

        hero =
            game.hero

        nearSpot =
            Grid.neighbors8 enemy.pos
                |> List.filter (\p -> Level.isPassableAt p game.level && p /= game.hero.pos)
                |> List.head
                |> Maybe.withDefault enemy.pos

        withScroll g =
            case Content.findItem "scroll-upgrade" g.ruleset of
                Just up ->
                    { g | items = { def = up, pos = nearSpot } :: g.items }

                Nothing ->
                    g
    in
    withScroll { game | hero = { hero | gold = hero.gold + bonusGold }, seed = seed1 }
        |> addLog ("You loot " ++ String.fromInt bonusGold ++ " gold from the fallen champion.")


{-| A slain monster has a good chance to leave a healing **dewdrop** on its cell (Shattered Pixel's
between-fights sustain). Walking over it restores a little HP — see `Items.pickUpItem`. -}
dewItem : ItemDef
dewItem =
    { id = "dew", name = "dewdrop", glyph = "˚", color = "#9be0ff", kind = Content.Consumable (HealHp 0), minDepth = 1, maxDepth = 0, spawnWeight = 0 }


maybeDropDew : Enemy -> Game -> Game
maybeDropDew enemy game =
    let
        ( roll, s1 ) =
            Rng.int 100 game.seed
    in
    if roll < 40 && not enemy.ally then
        { game | items = { def = dewItem, pos = enemy.pos } :: game.items, seed = s1 }

    else
        { game | seed = s1 }


heroAttack : Enemy -> Game -> Game
heroAttack enemy game =
    let
        -- A foe that hasn't noticed you (unaware), is asleep, or that you strike from cover (you stand
        -- in tall grass) takes a surprise attack for doubled damage.
        surprised =
            not enemy.alerted
                || List.any (\s -> s.kind == Paralyzed) enemy.statuses
                || (Level.at game.hero.pos game.level == Grass)

        -- To-hit: a surprise strike always lands; otherwise roll accuracy against the foe's evasion.
        ( hit, seedHit ) =
            if surprised then
                ( True, game.seed )

            else
                hitRoll (heroAccuracy game.hero) (enemyEvasion enemy.def) game.seed

        ( critRoll, seedC ) =
            Rng.int 12 seedHit

        crit =
            critRoll == 0

        ( base, seed1 ) =
            rollDamage (heroDamage game.hero) enemy.def.defense seedC

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

        -- A Vulnerable foe takes 50% more; a critical hit adds another half on top.
        vulnerable =
            if List.any (\s -> s.kind == Vulnerable) enemy.statuses then
                raw * 3 // 2

            else
                raw

        dmg =
            if crit then
                vulnerable * 3 // 2

            else
                vulnerable

        color =
            if crit then
                "#ff4d4d"

            else if surprised then
                "#ff7adf"

            else
                "#ffd166"

        remaining =
            enemy.hp - dmg

        surpriseNote =
            (if surprised then
                " (surprise!)"

             else
                ""
            )
                ++ (if crit then
                        " (critical!)"

                    else
                        ""
                   )
    in
    if not hit then
        { game | seed = seedHit, enemies = updateEnemyAt enemy.pos (\e -> { e | alerted = True }) game.enemies }
            |> addLog ("You miss the " ++ enemy.def.name ++ ".")
            |> addPopup enemy.pos "miss" "#9aa7ba"

    else if remaining <= 0 && enemy.revives > 0 then
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
            |> maybeDropDew enemy
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

        "chilling" ->
            -- Frost-bound: the struck foe is slowed.
            { game | enemies = updateEnemyAt enemy.pos (\e -> { e | statuses = addEnemyStatus Slowed 1 4 e.statuses }) game.enemies }

        "shocking" ->
            -- Lightning arcs to one foe beside the target for half the blow (never a killing jolt).
            case game.enemies |> List.filter (\e -> e.pos /= enemy.pos && Grid.chebyshev e.pos enemy.pos <= 1 && not e.ally) |> List.head of
                Just other ->
                    let
                        zap =
                            max 1 (dmg // 2)
                    in
                    { game | enemies = updateEnemyAt other.pos (\e -> { e | hp = max 1 (e.hp - zap), alerted = True }) game.enemies }
                        |> addPopup other.pos (String.fromInt zap) "#9be0ff"

                Nothing ->
                    game

        "lucky" ->
            -- A lucky strike now and then shakes loose a little coin.
            let
                ( roll, s1 ) =
                    Rng.int 100 game.seed
            in
            if roll < 20 then
                let
                    ( bonus, s2 ) =
                        Rng.range 1 (2 + game.depth) s1
                in
                { game | hero = { hero | gold = hero.gold + bonus }, seed = s2 }
                    |> addPopup game.hero.pos ("+" ++ String.fromInt bonus ++ "g") "#ffd166"

            else
                { game | seed = s1 }

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


