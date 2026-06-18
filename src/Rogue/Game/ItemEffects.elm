module Rogue.Game.ItemEffects exposing (..)

{-| The item-effect interpreter (applyEffect) — the engine half of the consumable DSL — plus the
identification of used items and the discovery-journal projection. Below Items in the layering. -}

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
            -- Potion of Strength: +n STR (which lifts both melee damage and accuracy) and a little vitality.
            { game | hero = { hero | str = hero.str + n, maxHp = hero.maxHp + n, hp = hero.hp + n } }
                |> addLog ("You drink the " ++ name ++ ". You feel stronger!")

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




-- moved down from Actions ------------------------------------------------------------------------

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

