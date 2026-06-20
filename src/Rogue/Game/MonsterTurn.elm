module Rogue.Game.MonsterTurn exposing (..)

{-| The monster turn and end-of-turn resolution: enemy AI/movement, FOV refresh, and endTurn
(which runs the monsters then ticks the environment). Above Combat and Sim. -}

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


-- MONSTER TURN -----------------------------------------------------------------------------------


{-| Every living monster acts once, in list order, after a hero action that consumed a turn. Each
either attacks the adjacent hero, steps toward a hero it can see, or idles. Positions are threaded
through an `occupied` set so monsters never stack. -}
enemiesTurnPass : Bool -> Game -> Game
enemiesTurnPass fastOnly game =
    if game.gameOver then
        game

    else
        let
            occupied0 =
                Set.fromList (( game.hero.pos.x, game.hero.pos.y ) :: List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies)

            ( newEnemiesRev, acc ) =
                List.foldl stepEnemy ( [], { hero = game.hero, seed = game.seed, log = game.log, occupied = occupied0, level = game.level, glassCannon = List.member "glass-cannon" game.challenges, badderBosses = List.member "badder-bosses" game.challenges, foes = game.enemies |> List.filter (\e -> not e.ally) |> List.map .pos, tempo = game.tempo, fastOnly = fastOnly } ) game.enemies
        in
        checkHeroDeath
            { game
                | enemies = List.reverse newEnemiesRev
                , hero = acc.hero
                , seed = acc.seed
                , log = acc.log
            }


type alias TurnAcc =
    { hero : Hero
    , seed : Seed
    , log : List String
    , occupied : Set ( Int, Int )
    , level : Level
    , glassCannon : Bool
    , badderBosses : Bool
    , foes : List Pos
    , tempo : Int
    , fastOnly : Bool
    }


{-| The nearest non-ally foe position to an ally (for it to hunt), from the turn's foe snapshot. -}
nearestFoeOf : Enemy -> TurnAcc -> Maybe Pos
nearestFoeOf enemy acc =
    acc.foes
        |> List.foldl
            (\p best ->
                case best of
                    Nothing ->
                        Just p

                    Just b ->
                        if Grid.chebyshev p enemy.pos < Grid.chebyshev b enemy.pos then
                            Just p

                        else
                            best
            )
            Nothing


{-| One monster's turn. It wakes on line of sight within `aggroRange` and stays alert thereafter
(remembering the hero). An alert monster: flees when badly hurt, melees an adjacent hero, shoots a hero
in range/sight if it has a ranged attack, or BFS-paths toward the hero (rounding corners). -}
stepEnemy : Enemy -> ( List Enemy, TurnAcc ) -> ( List Enemy, TurnAcc )
stepEnemy enemy ( done, acc ) =
    if acc.fastOnly && enemy.def.speed < 2 then
        ( enemy :: done, acc )

    else if not acc.fastOnly && enemy.def.speed <= 0 && modBy 2 acc.tempo == 1 then
        ( enemy :: done, acc )

    else
        stepEnemyAct enemy ( done, acc )


stepEnemyAct : Enemy -> ( List Enemy, TurnAcc ) -> ( List Enemy, TurnAcc )
stepEnemyAct enemy ( done, acc ) =
    let
        heroPos =
            acc.hero.pos

        healed =
            applyRegen enemy

        dist =
            Grid.chebyshev healed.pos heroPos

        heroHidden =
            hasStatus Invisible acc.hero

        los =
            Fov.visibleFrom healed.pos heroPos acc.level

        -- Crouching in tall grass halves how far a foe can first spot you (SPD stealth).
        sightRange =
            if Level.at heroPos acc.level == Grass then
                aggroRange // 2

            else
                aggroRange

        -- An invisible hero can't be acquired or tracked at range; only an adjacent foe still reacts.
        aware =
            if heroHidden then
                dist == 1

            else
                healed.alerted || (dist <= sightRange && los)

        woken =
            { healed | alerted = healed.alerted && not heroHidden || aware }
    in
    if List.any (\s -> s.kind == Paralyzed) enemy.statuses then
        -- Asleep / paralysed: it idles this turn (its status counts down in tickEnemyStatuses).
        ( woken :: done, acc )

    else if enemy.ally then
        -- A corrupted ally hunts the nearest foe (the `allyCombat` pass deals the blows when adjacent).
        case nearestFoeOf enemy acc of
            Just foePos ->
                if Grid.chebyshev enemy.pos foePos <= 1 then
                    ( enemy :: done, acc )

                else
                    moveEnemy enemy enemy (Path.firstStep acc.level acc.occupied enemy.pos foePos) done acc

            Nothing ->
                moveEnemy enemy enemy (Path.firstStep acc.level acc.occupied enemy.pos heroPos) done acc

    else if not aware then
        ( woken :: done, acc )

    else
        let
            -- A boss or a necromancer may summon a minion before acting.
            ( done1, acc1 ) =
                if woken.def.boss || woken.def.ability == Content.SummonsAllies then
                    trySummon woken done acc

                else
                    ( done, acc )

            -- An aquatic monster (piranha) only advances through water; on land it lurks in place.
            stepCell =
                Path.firstStep acc1.level acc1.occupied enemy.pos heroPos

            landlocked =
                woken.def.ability
                    == Content.Aquatic
                    && (case stepCell of
                            Just p ->
                                Level.at p acc1.level /= Water

                            Nothing ->
                                True
                       )
        in
        if dist == 1 && woken.def.ranged > 0 && los then
            -- Kiting: a ranged attacker backs off to keep its distance, shooting from afar instead of
            -- being cornered in melee. If it can't retreat, it shoots point-blank.
            case stepAway enemy.pos heroPos acc1.level acc1.occupied of
                Just away ->
                    moveEnemy enemy woken (Just away) done1 acc1

                Nothing ->
                    attackHero woken (enemy.def.name ++ " shoots you") done1 acc1

        else if dist == 1 then
            case woken.def.ability of
                Content.StealsGold amount ->
                    stealAndFlee woken amount done1 acc1

                _ ->
                    attackHero woken (enemy.def.name ++ " hits you") done1 acc1

        else if isFleeing woken then
            moveEnemy enemy woken (stepAway enemy.pos heroPos acc1.level acc1.occupied) done1 acc1

        else if woken.def.ranged > 0 && dist <= woken.def.ranged && los then
            attackHero woken (enemy.def.name ++ " shoots you") done1 acc1

        else if landlocked then
            ( woken :: done1, acc1 )

        else
            moveEnemy enemy woken stepCell done1 acc1


{-| A boss occasionally summons a weak minion onto a free adjacent cell. -}
trySummon : Enemy -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
trySummon boss done acc =
    let
        ( roll, seed1 ) =
            Rng.int 100 acc.seed

        free =
            Grid.neighbors8 boss.pos
                |> List.filter (\p -> Level.isPassableAt p acc.level && not (Set.member ( p.x, p.y ) acc.occupied))
                |> List.head
    in
    case ( roll < 12, free ) of
        ( True, Just spot ) ->
            let
                def =
                    minionDef boss.def

                minion =
                    { def = def, pos = spot, hp = def.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
            in
            ( minion :: done
            , { acc
                | seed = seed1
                , occupied = Set.insert ( spot.x, spot.y ) acc.occupied
                , log = ("The " ++ boss.def.name ++ " summons a minion!") :: acc.log
              }
            )

        _ ->
            ( done, { acc | seed = seed1 } )


{-| A weakened spawn derived from a boss. -}
minionDef : EnemyDef -> EnemyDef
minionDef bossDef =
    { bossDef
        | name = bossDef.name ++ " spawn"
        , maxHp = max 4 (bossDef.maxHp // 6)
        , damage = max 1 (bossDef.damage // 2)
        , defense = 0
        , ranged = 0
        , ability = Content.NoAbility
        , boss = False
        , xp = 1
        , glyph = String.toLower bossDef.glyph
    }


{-| A `Regenerates` monster heals a little at the start of its turn. -}
applyRegen : Enemy -> Enemy
applyRegen enemy =
    case enemy.def.ability of
        Content.Regenerates n ->
            { enemy | hp = min enemy.def.maxHp (enemy.hp + n) }

        _ ->
            enemy


{-| Pack tactics: a roused monster the hero can see alerts its packmates within three cells, so groups
wake and close in together rather than one at a time. -}
packAlert : Game -> Game
packAlert game =
    let
        sources =
            game.enemies
                |> List.filter (\e -> e.alerted && not e.ally && Set.member ( e.pos.x, e.pos.y ) game.visible)
                |> List.map .pos
    in
    if List.isEmpty sources then
        game

    else
        { game
            | enemies =
                List.map
                    (\e ->
                        if not e.alerted && not e.ally && List.any (\p -> Grid.chebyshev p e.pos <= 3) sources then
                            { e | alerted = True }

                        else
                            e
                    )
                    game.enemies
        }


{-| Corrupted allies strike: each ally adjacent to a non-ally foe wounds it (killing if it drops),
clearing slain foes without awarding the hero XP. -}
allyCombat : Game -> Game
allyCombat game =
    let
        allies =
            List.filter (\e -> e.ally) game.enemies

        attackedPositions =
            allies
                |> List.filterMap
                    (\a ->
                        game.enemies
                            |> List.filter (\e -> not e.ally && Grid.chebyshev e.pos a.pos == 1)
                            |> List.head
                            |> Maybe.map (\foe -> ( foe.pos, a.def.damage ))
                    )

        damageAt pos dmg enemies =
            List.filterMap
                (\e ->
                    if e.pos == pos && not e.ally then
                        if e.hp - dmg <= 0 then
                            Nothing

                        else
                            Just { e | hp = e.hp - dmg, alerted = True }

                    else
                        Just e
                )
                enemies

        resolved =
            List.foldl (\( pos, dmg ) es -> damageAt pos dmg es) game.enemies attackedPositions
    in
    if List.isEmpty attackedPositions then
        game

    else
        { game | enemies = resolved }


{-| An enraged boss (below half HP) occasionally unleashes a signature blast of paralytic gas around
itself — a slam shockwave / web burst that makes its arena dangerous to stand near. -}
applyBossHazards : Game -> Game
applyBossHazards game =
    let
        enragedBoss =
            game.enemies
                |> List.filter (\e -> e.def.boss && e.alerted && e.hp * 2 < e.def.maxHp)
                |> List.head
    in
    case enragedBoss of
        Just boss ->
            let
                ( roll, seed1 ) =
                    Rng.int 100 game.seed
            in
            if roll < 22 then
                bossSignature boss { game | seed = seed1 }

            else
                { game | seed = seed1 }

        Nothing ->
            game


{-| Caster auras: a `Heals` monster (a shaman) mends every nearby ally (and itself) each turn. -}
applyEnemyAuras : Game -> Game
applyEnemyAuras game =
    let
        healers =
            game.enemies
                |> List.filterMap
                    (\e ->
                        case e.def.ability of
                            Content.Heals n ->
                                Just ( e.pos, n )

                            _ ->
                                Nothing
                    )
    in
    if List.isEmpty healers then
        game

    else
        let
            mend e =
                let
                    bonus =
                        healers
                            |> List.filter (\( hp, _ ) -> Grid.chebyshev hp e.pos <= 3)
                            |> List.map Tuple.second
                            |> List.sum
                in
                if bonus > 0 && e.hp < e.def.maxHp then
                    { e | hp = min e.def.maxHp (e.hp + bonus) }

                else
                    e
        in
        { game | enemies = List.map mend game.enemies }


{-| Fungal monsters periodically belch a small spore cloud at their feet — a creeping toxic hazard
that pressures the hero to finish them quickly rather than trade blows in the haze. -}
applySpores : Game -> Game
applySpores game =
    let
        sporers =
            game.enemies
                |> List.filter (\e -> e.def.ability == Content.Spores && not e.ally)
    in
    if List.isEmpty sporers || modBy 3 game.turn /= 0 then
        game

    else
        List.foldl (\e g -> spawnGas ToxicGasCloud 2 e.pos g) game sporers


{-| A thief grabs gold — and snatches a consumable from your bag if it can — then bolts for the exit. -}
stealAndFlee : Enemy -> Int -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
stealAndFlee enemy amount done acc =
    let
        hero =
            acc.hero

        stolen =
            min amount hero.gold

        isConsumable it =
            case it.kind of
                Content.Consumable _ ->
                    True

                _ ->
                    False

        ( newInv, snatchLog ) =
            case findIndex isConsumable hero.inventory of
                Just i ->
                    ( removeAt i hero.inventory
                    , case nth i hero.inventory of
                        Just it ->
                            " and snatches your " ++ it.name ++ "!"

                        Nothing ->
                            " and flees!"
                    )

                Nothing ->
                    ( hero.inventory, " and flees!" )
    in
    ( { enemy | fleeing = True } :: done
    , { acc
        | hero = { hero | gold = hero.gold - stolen, inventory = newInv }
        , log = ("The " ++ enemy.def.name ++ " steals " ++ String.fromInt stolen ++ " gold" ++ snatchLog) :: acc.log
      }
    )


{-| A monster flees when badly hurt or after stealing — but bosses never flee, they fight to the end. -}
isFleeing : Enemy -> Bool
isFleeing enemy =
    not enemy.def.boss && (enemy.fleeing || enemy.hp * 4 < enemy.def.maxHp)


attackHero : Enemy -> String -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
attackHero enemy verb done acc =
    let
        -- An enraged boss (below half HP, or always with the Badder Bosses challenge) hits harder.
        enrage =
            if enemy.def.boss && (acc.badderBosses || enemy.hp * 2 < enemy.def.maxHp) then
                3

            else
                0

        weaken =
            if List.any (\s -> s.kind == Weakened) enemy.statuses then
                2

            else
                0

        ( hit, seedHit ) =
            hitRoll (enemyAccuracy enemy.def) (heroEvasion acc.hero) acc.seed

        ( rawHit, seedB ) =
            rollDamage (max 1 (enemy.def.damage + enrage - weaken)) 0 seedHit

        -- Armour blocks a random amount up to its rating (SPD-style mitigation variance).
        ( block, seed1 ) =
            Rng.int (heroDefense acc.hero + 1) seedB

        rolled =
            max 1 (rawHit - block)

        glassed =
            if acc.glassCannon then
                rolled * 2

            else
                rolled

        dmg =
            if hasStatus Vulnerable acc.hero then
                glassed * 3 // 2

            else
                glassed

        hero =
            acc.hero

        -- A blazing champion sets the hero alight on a hit; a warlock's hex saps strength.
        ( burnt, burnLog ) =
            if enemy.def.ability == Content.Burns then
                ( addEnemyStatus Burn 3 3 hero.statuses, " You catch fire!" )

            else if enemy.def.ability == Content.Weakens then
                ( addEnemyStatus Weakened 1 5 hero.statuses, " A hex saps your strength!" )

            else if enemy.def.ability == Content.Charms then
                ( addEnemyStatus Charmed 1 4 hero.statuses, " You are charmed!" )

            else
                ( hero.statuses, "" )

        -- Thorns armour reflects a barb of damage back at the attacker.
        ( reflectedEnemy, thornLog ) =
            if itemEnchant hero.armour == "thorns" then
                ( { enemy | hp = enemy.hp - 3 }, " Thorns bite back!" )

            else
                ( enemy, "" )
    in
    if not hit then
        ( enemy :: done
        , { acc | seed = seedHit, log = ("The " ++ enemy.def.name ++ " attacks — you dodge!") :: acc.log }
        )

    else
        ( reflectedEnemy :: done
        , { acc
            | hero = { hero | hp = hero.hp - dmg, statuses = burnt }
            , seed = seed1
            , log = ("The " ++ verb ++ " (" ++ String.fromInt dmg ++ ")." ++ burnLog ++ thornLog) :: acc.log
          }
        )


moveEnemy : Enemy -> Enemy -> Maybe Pos -> List Enemy -> TurnAcc -> ( List Enemy, TurnAcc )
moveEnemy original woken maybeNext done acc =
    case maybeNext of
        Just next ->
            ( { woken | pos = next } :: done
            , { acc
                | occupied =
                    acc.occupied
                        |> Set.remove ( original.pos.x, original.pos.y )
                        |> Set.insert ( next.x, next.y )
              }
            )

        Nothing ->
            ( woken :: done, acc )


{-| Step to the passable, unoccupied neighbour that most *increases* distance from the target. -}
stepAway : Pos -> Pos -> Level -> Set ( Int, Int ) -> Maybe Pos
stepAway from to level occupied =
    Grid.eightDirs
        |> List.map (Grid.move from)
        |> List.filter (\p -> Level.isPassableAt p level && not (Set.member ( p.x, p.y ) occupied))
        |> maximumBy (\p -> Grid.chebyshev p to)


enemyAt : Pos -> Game -> Maybe Enemy
enemyAt p game =
    listFind (\e -> e.pos == p) game.enemies


refreshFov : Game -> Game
refreshFov game =
    let
        darkness =
            if List.member "darkness" game.challenges then
                2

            else
                0

        radius =
            max 2 (fovRadiusFor game.hero game.depth - darkness)

        vis =
            withTorchlight game.level (Fov.compute radius game.hero.pos game.level)
    in
    { game | visible = vis, explored = Set.union game.explored vis }


{-| Firelight extends sight: a torch the hero can already see also lights the room it *faces*, so the
glow of a sconce helps you make out a little more than you otherwise could. Folded into the visible set.

A torch sits in a wall, so we can't cast its light from the torch cell itself — that would leak through
the wall and reveal the empty rock behind it. Instead each visible torch lights the room from its open,
already-seen neighbours (the side the sconce faces); casting from those floor cells, walls properly stop
the light. -}
torchSightRadius : Int
torchSightRadius =
    3


withTorchlight : Level -> Set ( Int, Int ) -> Set ( Int, Int )
withTorchlight level vis =
    Level.torches level
        |> List.filter (\t -> Set.member ( t.x, t.y ) vis)
        |> List.foldl (\t acc -> Set.union acc (torchLit level vis t)) vis


torchLit : Level -> Set ( Int, Int ) -> Pos -> Set ( Int, Int )
torchLit level vis torch =
    Grid.eightDirs
        |> List.map (Grid.move torch)
        |> List.filter (\n -> Level.isPassableAt n level && Set.member ( n.x, n.y ) vis)
        |> List.foldl (\n acc -> Set.union acc (Fov.compute torchSightRadius n level)) Set.empty


enemiesTurn : Game -> Game
enemiesTurn game =
    enemiesTurnPass False game


{-| Close out a turn-consuming hero action: run the monsters, then tick the counter. -}
endTurn : Game -> Game
endTurn game =
    let
        afterStatuses =
            tickStatuses game

        tempo =
            afterStatuses.tempo + 1

        bumped =
            { afterStatuses | tempo = tempo }

        -- Haste lets the hero act on every tick while monsters act on every other; slow does the
        -- reverse (monsters get a bonus turn). Neither → the usual one-for-one.
        enemyPhases =
            if hasStatus Hasted bumped.hero || Maybe.map .id bumped.hero.ring == Just "ring-haste" then
                modBy 2 tempo

            else if hasStatus Slowed bumped.hero || hasStatus Crippled bumped.hero then
                2

            else
                1

        afterEnemyDot =
            tickEnemyStatuses bumped

        afterAuras =
            applySpores (applyEnemyAuras afterEnemyDot)

        afterHazards =
            applyBossHazards afterAuras

        afterAllies =
            allyCombat afterHazards

        afterPack =
            packAlert afterAllies

        afterStatues =
            animateStatues afterPack

        afterMonsters =
            applyTimes enemyPhases enemiesTurn afterStatues

        afterFast =
            enemiesTurnPass True afterMonsters

        afterGas =
            tickGas afterFast

        afterFire =
            tickFire afterGas

        afterIce =
            tickIce afterFire

        afterPerception =
            passivePerception afterIce

        afterHunger =
            tickHunger afterPerception

        recharged =
            rechargeAbility (rechargeWands { afterHunger | turn = afterHunger.turn + 1 })
    in
    checkQuest (maybeWander (ambientEvent (cursedBackfire (ringRegen recharged))))


{-| The Ring of Regeneration knits wounds over time: 1 HP back every few turns while it's worn. -}
ringRegen : Game -> Game
ringRegen game =
    let
        wearing =
            case game.hero.ring of
                Just r ->
                    r.id == "ring-regen"

                Nothing ->
                    False

        hero =
            game.hero
    in
    if wearing && hero.hp < hero.maxHp && modBy 5 game.turn == 0 then
        { game | hero = { hero | hp = min hero.maxHp (hero.hp + 1) } }

    else
        game


{-| Deep floors are unstable: an occasional ambient hazard (a quake raining rubble, or a vent of gas)
keeps the depths tense even between fights. Only fires from the Caves down (depth >= 6). -}
ambientEvent : Game -> Game
ambientEvent game =
    if game.depth < 6 || game.gameOver then
        game

    else
        let
            ( fires, seed1 ) =
                Rng.chance 3 game.seed
        in
        if not fires then
            { game | seed = seed1 }

        else
            let
                ( pick, seed2 ) =
                    Rng.int 2 seed1
            in
            if pick == 0 then
                let
                    ( dmg, seed3 ) =
                        Rng.range 2 (2 + game.depth // 3) seed2
                in
                checkHeroDeath
                    (damageHero dmg { game | seed = seed3 }
                        |> addLog ("The floor heaves — falling rubble strikes you! (" ++ String.fromInt dmg ++ ")")
                    )

            else
                spawnGas ToxicGasCloud 3 game.hero.pos { game | seed = seed2 }
                    |> addLog "A vent cracks open, hissing toxic gas into the air!"


{-| Cursed worn gear occasionally lashes out: a small chance each turn of a malign hiccup (a sting of
damage or a brief debuff). The more cursed pieces equipped, the likelier it bites. -}
cursedBackfire : Game -> Game
cursedBackfire game =
    let
        cursedCount =
            [ game.hero.weapon, game.hero.armour, game.hero.ring ]
                |> List.filterMap identity
                |> List.filter isCursed
                |> List.length
    in
    if cursedCount == 0 then
        game

    else
        let
            ( fires, seed1 ) =
                Rng.chance (4 * cursedCount) game.seed
        in
        if not fires then
            { game | seed = seed1 }

        else
            let
                ( which, seed2 ) =
                    Rng.int 3 seed1
            in
            case which of
                0 ->
                    checkHeroDeath (damageHero 2 { game | seed = seed2 } |> addLog "Your cursed gear bites into you!")

                1 ->
                    addStatus Weakened 1 4 { game | seed = seed2 } |> addLog "A curse saps your strength."

                _ ->
                    addStatus Slowed 1 3 { game | seed = seed2 } |> addLog "Cursed gear drags at your limbs."


{-| Build the hero's class-ability charge by one each turn, up to its maximum. -}
rechargeAbility : Game -> Game
rechargeAbility game =
    let
        hero =
            game.hero
    in
    if hero.abilityCharge < abilityMax then
        { game | hero = { hero | abilityCharge = hero.abilityCharge + 1 } }

    else
        game


{-| Deliver the imp's bounty once its kill target is reached. -}
checkQuest : Game -> Game
checkQuest game =
    case game.quest of
        Just quest ->
            if (quest.targetKills > 0 && game.kills >= quest.targetKills) || (quest.targetDepth > 0 && game.depth >= quest.targetDepth) then
                let
                    hero =
                        game.hero
                in
                { game | quest = Nothing, hero = { hero | inventory = hero.inventory ++ [ quest.reward ] } }
                    |> addLog ("The " ++ quest.giver ++ "'s bounty is fulfilled — " ++ withArticle (displayName game.idents quest.reward) ++ " appears in your pack!")

            else
                game

        Nothing ->
            game


{-| Recharge carried relics each turn: wands regain a charge every 12 turns; artifacts build one
charge per turn toward their maximum. -}
rechargeWands : Game -> Game
rechargeWands game =
    let
        hero =
            game.hero

        -- The Ring of Energy quickens every charge: wands tick up faster and artifacts gain double.
        energized =
            case hero.ring of
                Just r ->
                    r.id == "ring-power"

                Nothing ->
                    False

        wandTick =
            modBy
                (if energized then
                    8

                 else
                    12
                )
                game.turn
                == 0

        artifactGain =
            if energized then
                2

            else
                1

        bumped =
            List.map
                (\it ->
                    case it.kind of
                        Content.Wand spec ->
                            if wandTick && spec.charges < spec.maxCharges then
                                { it | kind = Content.Wand { spec | charges = spec.charges + 1 } }

                            else
                                it

                        Content.Artifact spec ->
                            if spec.charge < spec.maxCharge then
                                { it | kind = Content.Artifact { spec | charge = min spec.maxCharge (spec.charge + artifactGain) } }

                            else
                                it

                        _ ->
                            it
                )
                hero.inventory
    in
    { game | hero = { hero | inventory = bumped } }


{-| Every so often a fresh monster wanders onto the floor — out of the hero's sight — so camping isn't
free. Capped a little above the floor's normal population. -}
maybeWander : Game -> Game
maybeWander game =
    let
        -- The Amulet's bearer is hunted: waves come twice as fast, larger, and already awake.
        -- The Swarming challenge floods every floor the same way.
        interval =
            if game.ascending || List.member "swarming" game.challenges then
                20

            else
                40

        cap =
            Content.spawnCountForDepth game.depth
                + (if game.ascending then
                    9

                   else
                    3
                  )
    in
    if game.gameOver || modBy interval game.turn /= 0 || List.length game.enemies >= cap then
        game

    else
        let
            candidates =
                Content.enemiesForDepth game.depth game.ruleset

            occupied =
                Set.insert ( game.hero.pos.x, game.hero.pos.y ) (Set.fromList (List.map (\e -> ( e.pos.x, e.pos.y )) game.enemies))

            spots =
                Level.positions game.level
                    |> List.filter
                        (\p ->
                            Level.at p game.level
                                == Floor
                                && not (Set.member ( p.x, p.y ) game.visible)
                                && not (Set.member ( p.x, p.y ) occupied)
                        )
        in
        case ( candidates, spots ) of
            ( ( _, firstDef ) :: _, _ ) ->
                let
                    ( spot, s1 ) =
                        Rng.pick game.hero.pos spots game.seed

                    ( def, s2 ) =
                        Rng.pickWeighted firstDef candidates s1
                in
                if List.isEmpty spots then
                    game

                else
                    { game
                        | enemies = { def = def, pos = spot, hp = def.maxHp, alerted = game.ascending, fleeing = False, statuses = [], ally = False, revives = 0 } :: game.enemies
                        , seed = s2
                    }

            _ ->
                game



-- STATUE GUARDIANS (moved from Rogue.Game) --------------------------------------------------------

animateStatues : Game -> Game
animateStatues game =
    let
        ( woken, dormant ) =
            List.partition (\p -> Grid.chebyshev p game.hero.pos <= 2) game.statues
    in
    if List.isEmpty woken then
        game

    else
        let
            guards =
                List.map
                    (\p ->
                        let
                            def =
                                guardianDef game.depth
                        in
                        { def = def, pos = p, hp = def.maxHp, alerted = True, fleeing = False, statuses = [], ally = False, revives = 0 }
                    )
                    woken
        in
        { game | statues = dormant, enemies = guards ++ game.enemies }
            |> addLog "A statue grinds to life — a guardian attacks!"

guardianDef : Int -> EnemyDef
guardianDef depth =
    { id = "guardian"
    , name = "animated guardian"
    , glyph = "8"
    , color = "#b0b0b8"
    , maxHp = 26 + depth * 3
    , damage = 7 + depth // 2
    , defense = 5
    , speed = 1
    , ranged = 0
    , ability = Content.NoAbility
    , boss = False
    , minDepth = 1
    , maxDepth = 99
    , spawnWeight = 0
    , xp = 8 + depth
    , drop = Nothing
    }



{-| Each boss's signature desperation move (fired by `applyBossHazards` once enraged): the Spider
Queen ensnares you in webs, the Dwarf King brings the ceiling down, Yog-Dzewa calls down hellfire,
and the rest loose a paralysing burst. -}
bossSignature : Enemy -> Game -> Game
bossSignature boss game =
    case boss.def.id of
        "spider-queen" ->
            let
                webs =
                    Grid.neighbors8 game.hero.pos
                        |> List.filter (\p -> Level.isPassableAt p game.level)
                        |> List.take 3
                        |> List.map (\p -> { pos = p, kind = Web, revealed = True })
            in
            addStatus Crippled 1 5 { game | traps = webs ++ game.traps }
                |> addLog ("The " ++ boss.def.name ++ " spins a web around you!")

        "dwarf-king" ->
            let
                ( dmg, s1 ) =
                    Rng.range game.depth (game.depth + 5) game.seed
            in
            checkHeroDeath (damageHero dmg { game | seed = s1 } |> addLog ("The " ++ boss.def.name ++ " brings the ceiling crashing down! (" ++ String.fromInt dmg ++ ")"))

        "yog-dzewa" ->
            spawnFire game.hero.pos game
                |> addLog ("The " ++ boss.def.name ++ " calls down hellfire!")

        _ ->
            spawnGas ParalyticGasCloud 4 boss.pos game
                |> addLog ("The " ++ boss.def.name ++ " unleashes a paralysing burst!")
