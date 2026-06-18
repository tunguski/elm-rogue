module Rogue.Game.Items exposing (..)

{-| Using, throwing, dropping, selling and brewing items, the quickslot/inventory plumbing, and
alchemy. Calls into ItemEffects to actually resolve a consumable's effect. -}

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
import Rogue.Game.Combat exposing (..)
import Rogue.Game.Sim exposing (..)
import Rogue.Game.MonsterTurn exposing (..)
import Rogue.Game.ItemEffects exposing (..)


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

    else if it.def.id == "dew" then
        -- Dewdrops aren't carried — they restore a little HP the instant you walk over them.
        let
            hero =
                game.hero

            heal =
                1 + game.depth // 2
        in
        if hero.hp >= hero.maxHp then
            game

        else
            { game | hero = { hero | hp = min hero.maxHp (hero.hp + heal) } }
                |> addLog "You absorb a refreshing dewdrop."

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