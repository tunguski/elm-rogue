module Rogue.Game exposing
    ( Game
    , Hero
    , Enemy
    , Msg(..)
    , SaveData
    , newGame
    , resume
    , knownItemIds
    , chooseSubclass
    , learnTalent
    , subclassChoices
    , talentChoices
    , startChallenges
    , challengeChoices
    , alchemyRecipes
    , itemCatalog
    , nearShop
    , takeGhostGift
    , chooseEnchant
    , setRemains
    , update
    , toScene
    )

{-| The game state and its pure update — the heart of the engine.

`Game` is a plain record (level, hero, the monsters in play, turn/depth counters, the seen/explored
fog sets, a message log, a deterministic seed and the active `Ruleset`) and `update` maps a player
`Msg` to the next `Game`. Everything is pure and seed-threaded, so the whole engine runs head-lessly
and a replay of the same inputs from the same seed is identical.

The engine reads all its content from the injected `Rogue.Content.Ruleset`: hero stats, the bestiary
and (later) items. That is the moddability contract — swap the ruleset and the same engine plays a
different game.

`toScene` projects a `Game` onto the renderer-agnostic `Rogue.Render.Scene`, the only thing any
renderer ever sees.

Milestones 4–6: hero movement, the turn counter, fog of war and a data-driven monster population.
Combat and AI arrive in M7.
-}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (EnemyDef, ItemDef, ItemEffect(..), Ruleset)
import Rogue.Dungeon as Dungeon exposing (Generated, Room)
import Rogue.Fov as Fov
import Rogue.Grid as Grid exposing (Dir, Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Path as Path
import Rogue.Render as Render exposing (Scene)
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
import Rogue.Game.Actions exposing (..)
import Rogue.Game.Scene exposing (toScene)


type Msg
    = Move Dir
    | Descend
    | Wait
    | Use Int
    | Search
    | Fire
    | ThrowAt Pos
    | Brew
    | Ability
    | Examine
    | AutoExplore
    | Drop Int
    | Sell Int
    | Restart
    | NoOp


{-| Start a fresh run at depth 1 from a ruleset, a chosen class and a numeric seed. The class sets
the hero's stats and opening gear (resolved against the ruleset's item list). -}
newGame : Ruleset -> Content.ClassDef -> Int -> Game
newGame ruleset class rawSeed =
    let
        gen =
            Dungeon.generate (Dungeon.configForDepth 1) (Rng.seed rawSeed)

        resolve maybeId =
            Maybe.andThen (\id -> Content.findItem id ruleset) maybeId

        startingInventory =
            List.filterMap (\id -> Content.findItem id ruleset) class.startingItems

        hero =
            { pos = gen.stairsUp
            , hp = class.maxHp
            , maxHp = class.maxHp
            , damage = class.damage
            , defense = class.defense
            , inventory = startingInventory
            , gold = 0
            , weapon = resolve class.startingWeapon
            , armour = resolve class.startingArmour
            , ring = Nothing
            , glyph = class.glyph
            , color = class.color
            , fovRadius = class.fovRadius
            , statuses = []
            , level = 1
            , xp = 0
            , nutrition = 400
            , subclass = Nothing
            , talents = []
            , heroClass = class.id
            , abilityCharge = 0
            , str = 10
            }
    in
    let
        ( looks, seedA ) =
            assignLooks ruleset gen.seed

        idents =
            { known = Set.empty, looks = looks }
    in
    enterLevel ruleset 1 0 idents seedA gen hero [ "You enter the dungeon as " ++ withArticle class.name ++ "." ]




{-| The ids of every item type the hero has identified — for persisting in a save. -}
knownItemIds : Game -> List String
knownItemIds game =
    Set.toList game.idents.known


{-| Continue a saved run: regenerate a floor at the saved depth and reinstate the hero (stats, gear
and inventory resolved from the ruleset). Within-floor progress isn't kept — you resume your character
at the start of a fresh floor at that depth. -}
resume : Ruleset -> SaveData -> Game
resume ruleset save =
    let
        gen =
            Dungeon.generate (Dungeon.configForDepth save.depth) (Rng.seed save.seed)

        find id =
            Content.findItem id ruleset

        hero =
            { pos = gen.stairsUp
            , hp = save.hp
            , maxHp = save.maxHp
            , damage = save.damage
            , defense = save.defense
            , inventory = List.filterMap find save.inventoryIds
            , gold = save.gold
            , weapon = Maybe.andThen find save.weaponId
            , armour = Maybe.andThen find save.armourId
            , ring = Maybe.andThen find save.ringId
            , glyph = save.glyph
            , color = save.color
            , fovRadius = save.fovRadius
            , statuses = []
            , level = save.level
            , xp = save.xp
            , nutrition = save.nutrition
            , subclass = Nothing
            , talents = []
            , heroClass = ""
            , abilityCharge = 0
            , str = 10
            }

        ( looks, seedA ) =
            assignLooks ruleset gen.seed

        idents =
            { known = Set.fromList save.knownIds, looks = looks }
    in
    enterLevel ruleset save.depth 0 idents seedA gen hero [ "You resume your delve on depth " ++ String.fromInt save.depth ++ "." ]


{-| Commit to a subclass (chosen at the depth threshold). Some subclasses grant an immediate bonus
(the Sentinel's extra vitality); all confer a passive read by `heroDamage`/`heroDefense`/`heroAttack`. -}
chooseSubclass : String -> Game -> Game
chooseSubclass name game =
    let
        hero =
            game.hero

        bonusHp =
            if name == "Sentinel" then
                8

            else
                0
    in
    { game | hero = { hero | subclass = Just name, maxHp = hero.maxHp + bonusHp, hp = hero.hp + bonusHp } }
        |> addLog ("You embrace the path of the " ++ name ++ "!")


{-| Learn a talent (chosen on level-up). `Toughness` raises max HP at once; the rest are passives. -}
learnTalent : String -> Game -> Game
learnTalent name game =
    let
        hero =
            game.hero

        bonusHp =
            case name of
                "Toughness" ->
                    5

                "Vitality" ->
                    10

                _ ->
                    0

        fovBonus =
            if name == "Keen Eye" then
                1

            else
                0

        learned =
            { hero
                | talents = name :: hero.talents
                , maxHp = hero.maxHp + bonusHp
                , hp =
                    if name == "Second Wind" then
                        hero.maxHp + bonusHp

                    else
                        hero.hp + bonusHp
                , fovRadius = hero.fovRadius + fovBonus
            }
    in
    { game | hero = learned }
        |> addLog ("You master the " ++ name ++ " talent.")


{-| Record the remains of a previous death on a fresh run, and place them immediately if the starting
floor is the right depth. -}
setRemains : Remains -> Game -> Game
setRemains r game =
    spawnRemainsIfHere { game | remains = Just r }


{-| If the hero has reached the depth where their predecessor fell, materialise the remains: recover
the gold at once and drop the saved item on the start cell, then clear the remains. -}
{-| Apply chosen run-modifier challenges to a fresh game (records them; some, like "Frailty", also
adjust the starting hero). Ids are checked at the relevant engine hooks. -}
startChallenges : List String -> Game -> Game
startChallenges ids game =
    let
        hero =
            game.hero

        frail =
            if List.member "frailty" ids then
                { hero | maxHp = max 8 (hero.maxHp - 8), hp = max 8 (hero.hp - 8) }

            else
                hero

        stripped =
            if List.member "minimalist" ids then
                { frail | inventory = [] }

            else
                frail

        bareHanded =
            if List.member "bare-handed" ids then
                { stripped | weapon = Nothing, armour = Nothing }

            else
                stripped
    in
    { game | challenges = ids, hero = bareHanded }


{-| The optional run-modifier challenges (id, label, one-line effect). -}
challengeChoices : List ( String, ( String, String ) )
challengeChoices =
    [ ( "no-healing", ( "Pharmacophobia", "healing potions do nothing" ) )
    , ( "glass-cannon", ( "Glass Cannon", "you deal and take double damage" ) )
    , ( "darkness", ( "Into Darkness", "your sight is dimmed (−2 FOV)" ) )
    , ( "frailty", ( "Frailty", "start with 8 less max HP" ) )
    , ( "no-scrolls", ( "Forbidden Runes", "scrolls crumble unread" ) )
    , ( "starvation", ( "On a Diet", "hunger sets in twice as fast" ) )
    , ( "badder-bosses", ( "Badder Bosses", "bosses are enraged from the start" ) )
    , ( "minimalist", ( "Minimalist", "begin with an empty pack" ) )
    , ( "bare-handed", ( "Bare-Handed", "begin with no weapon or armour" ) )
    , ( "swarming", ( "Swarming", "monsters wander in far more often" ) )
    ]


{-| The subclasses offered at the depth threshold (label + one-line effect). -}
subclassChoices : List ( String, String )
subclassChoices =
    [ ( "Duellist", "+2 damage to every strike" )
    , ( "Berserker", "+4 damage while badly wounded" )
    , ( "Sentinel", "+8 max HP and +2 defense" )
    , ( "Stalker", "surprise strikes deal triple" )
    ]


{-| The tiered talent tree: each talent has a minimum hero level (T1 from L2, T2 from L4, T3 from L7). -}
talentChoices : List { name : String, desc : String, minLevel : Int }
talentChoices =
    [ { name = "Sharpened Edge", desc = "+1 damage", minLevel = 2 }
    , { name = "Iron Will", desc = "+1 defense", minLevel = 2 }
    , { name = "Toughness", desc = "+5 max HP", minLevel = 2 }
    , { name = "Deadly Strike", desc = "+2 damage", minLevel = 4 }
    , { name = "Keen Eye", desc = "+1 sight radius", minLevel = 4 }
    , { name = "Vitality", desc = "+10 max HP", minLevel = 4 }
    , { name = "Heroism", desc = "+3 damage", minLevel = 7 }
    , { name = "Bulwark", desc = "+3 defense", minLevel = 7 }
    , { name = "Second Wind", desc = "heal fully now", minLevel = 7 }
    , { name = "Executioner", desc = "+4 damage", minLevel = 7 }
    , { name = "Bloodlust", desc = "+3 damage while below half HP", minLevel = 7 }
    , { name = "Last Stand", desc = "+4 defense while below half HP", minLevel = 7 }
    ]


{-| Place the hero on a freshly generated level, spawn its monster population, recompute fog, and keep
the carried-over hero, kill count and log. Shared by `newGame` and descending. -}-- UPDATE -----------------------------------------------------------------------------------------


update : Msg -> Game -> Game
update rawMsg rawGame =
    let
        -- Each new action starts with a clean set of floating numbers.
        game =
            if isActionMsg rawMsg then
                { rawGame | popups = [] }

            else
                rawGame

        msg =
            rawMsg
    in
    if game.gameOver then
        game

    else if hasStatus Paralyzed game.hero && isActionMsg msg then
        -- Paralysed: the action is lost but time still passes (monsters act, the status counts down).
        endTurn (addLog "You are paralysed and cannot move!" game)

    else if hasStatus Charmed game.hero && isActionMsg msg && charmFalters game then
        -- Charmed: now and then you stand transfixed, losing the action while the world moves on.
        endTurn (addLog "You stand transfixed, charmed!" { game | seed = Tuple.second (Rng.int 2 game.seed) })

    else
        case msg of
            Move dir ->
                tryMove dir game

            Descend ->
                if game.ascending then
                    tryAscend game

                else
                    tryDescend game

            Wait ->
                endTurn game

            Use index ->
                tryUse index game

            Search ->
                endTurn (searchTraps game)

            Fire ->
                tryFire game

            ThrowAt pos ->
                tryThrowAt pos game

            Brew ->
                tryBrew game

            Ability ->
                useAbility game

            Examine ->
                examine game

            AutoExplore ->
                autoExplore game

            Drop index ->
                dropItem index game

            Sell index ->
                sellItem index game

            Restart ->
                -- The shell (Main) owns reseeding a new run; inside a game it's a no-op.
                game

            NoOp ->
                game


{-| Does this message represent a turn-consuming hero action (the kind paralysis blocks)? -}
{-| Whether a charmed hero falters this action (≈50%), losing the turn to fascination. -}
charmFalters : Game -> Bool
charmFalters game =
    Tuple.first (Rng.chance 50 game.seed)


isActionMsg : Msg -> Bool
isActionMsg msg =
    case msg of
        Restart ->
            False

        NoOp ->
            False

        Examine ->
            False

        Drop _ ->
            False

        Sell _ ->
            False

        _ ->
            True


{-| Take one step toward the nearest unexplored floor cell (or the down-stairs once the floor is fully
mapped), pathing around walls. Stops with a warning if a monster is in view. -}