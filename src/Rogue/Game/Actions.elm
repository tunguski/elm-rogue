module Rogue.Game.Actions exposing (..)

{-| Player-action handlers: movement/bump-interaction, auto-explore, descending, plus the trap
machinery. The top engine layer below the Game dispatcher. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
import Rogue.Dungeon as Dungeon exposing (Generated, Room)
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
import Rogue.Game.Items exposing (..)
import Rogue.Game.Floor exposing (..)


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
            endTurn (heavyRecovery (heroAttack enemy game))

        Nothing ->
            -- Reach weapons (spear) strike a foe two cells away in a straight line, through the empty
            -- cell you'd have stepped into.
            case reachTarget dir game of
                Just farEnemy ->
                    endTurn (heroAttack farEnemy game)

                Nothing ->
                    moveOrInteract dir target game


{-| The enemy a reach weapon can hit: two cells out in `dir`, when the hero wields a reach weapon and
the intervening cell is passable and empty. -}
reachTarget : Dir -> Game -> Maybe Enemy
reachTarget dir game =
    if not (weaponReach game.hero.weapon) then
        Nothing

    else
        let
            mid =
                Grid.move game.hero.pos dir

            far =
                Grid.move mid dir
        in
        if Level.isPassableAt mid game.level && enemyAt mid game == Nothing then
            enemyAt far game |> Maybe.andThen (\e -> if e.ally then Nothing else Just e)

        else
            Nothing


{-| Does the wielded weapon have extended reach (hits at range 2)? -}
weaponReach : Maybe ItemDef -> Bool
weaponReach maybeWeapon =
    case maybeWeapon of
        Just w ->
            w.id == "spear" || w.id == "glaive" || w.id == "grim-glaive"

        Nothing ->
            False


{-| Heavy weapons hit hard but are unwieldy: each swing leaves the hero briefly Slowed, so foes get an
extra beat to act. The big base damage lives in the weapon's stats; this is the recovery cost. -}
heavyRecovery : Game -> Game
heavyRecovery game =
    let
        heavy =
            case game.hero.weapon of
                Just w ->
                    w.id == "warhammer" || w.id == "blazing-mace"

                Nothing ->
                    False
    in
    if heavy then
        addStatus Slowed 1 1 game

    else
        game


moveOrInteract : Dir -> Pos -> Game -> Game
moveOrInteract dir target game =
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
{-| A short-lived friendly bee, released from a honeypot, that harries nearby foes. -}
{-| A harmless sheep — from a polymorph scroll or a decoy. Deals no damage; as an ally it draws enemy
attacks (a living shield) and blocks corridors. -}
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

        Web ->
            addStatus Crippled 1 6 game |> addLog "Sticky webs spring from the floor and ensnare you!"

        PitfallTrap ->
            fallThroughChasm game


{-| Relocate the hero to a random passable cell (used by teleport traps) and refresh fog. -}
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


