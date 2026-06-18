module Rogue.Game.Sim exposing (..)

{-| Environmental simulation ticked each turn: gas clouds (spread/thin) and fire (spread/burn),
plus the hero status primitives. Sits above Combat (hazards deal damage). -}

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


-- GAS CLOUDS -------------------------------------------------------------------------------------


{-| Stamp a gas cloud (and a one-cell border) onto the passable cells around `center`. -}
spawnGas : GasKind -> Int -> Pos -> Game -> Game
spawnGas kind density center game =
    let
        cells =
            (center :: Grid.neighbors4 center)
                |> List.filter (\p -> Level.isPassableAt p game.level)

        added =
            List.foldl (\p d -> Dict.insert ( p.x, p.y ) { kind = kind, density = density } d) game.gas cells
    in
    { game | gas = added }


{-| Diffuse every gas cloud one cell outward and thin it by one, then apply its effect to whoever
stands in it. Each non-wall cell takes the strongest of (its own density - 1) and (a neighbour's
density - 1); at zero it clears. -}
tickGas : Game -> Game
tickGas game =
    if Dict.isEmpty game.gas then
        game

    else
        let
            at key =
                Dict.get key game.gas

            candidateKeys =
                Dict.keys game.gas
                    |> List.concatMap
                        (\( x, y ) ->
                            ( x, y ) :: List.map (\p -> ( p.x, p.y )) (Grid.neighbors4 { x = x, y = y })
                        )
                    |> Set.fromList
                    |> Set.toList

            sourcesFor ( x, y ) =
                (( x, y ) :: List.map (\p -> ( p.x, p.y )) (Grid.neighbors4 { x = x, y = y }))
                    |> List.filterMap (\k -> at k |> Maybe.map (\g -> { kind = g.kind, density = g.density - 1 }))

            next ( x, y ) =
                if Level.isPassableAt { x = x, y = y } game.level then
                    case maximumBy .density (sourcesFor ( x, y )) of
                        Just best ->
                            if best.density > 0 then
                                Just ( ( x, y ), { kind = best.kind, density = best.density } )

                            else
                                Nothing

                        Nothing ->
                            Nothing

                else
                    Nothing

            newGas =
                List.filterMap next candidateKeys
        in
        applyGasEffects { game | gas = Dict.fromList newGas }


{-| Apply the gas under the hero (status) and under each monster (status) this turn. -}
applyGasEffects : Game -> Game
applyGasEffects game =
    let
        hero =
            game.hero

        afterHero =
            case Dict.get ( hero.pos.x, hero.pos.y ) game.gas of
                Just g ->
                    addStatus (gasStatus g.kind) (gasMagnitude g.kind) 2 game
                        |> addLog (gasLog g.kind)

                Nothing ->
                    game

        affected e =
            case Dict.get ( e.pos.x, e.pos.y ) afterHero.gas of
                Just g ->
                    { e | statuses = addEnemyStatus (gasStatus g.kind) (gasMagnitude g.kind) 2 e.statuses, alerted = True }

                Nothing ->
                    e
    in
    { afterHero | enemies = List.map affected afterHero.enemies }


gasStatus : GasKind -> StatusKind
gasStatus kind =
    case kind of
        ParalyticGasCloud ->
            Paralyzed

        _ ->
            Poison


gasMagnitude : GasKind -> Int
gasMagnitude kind =
    case kind of
        ToxicGasCloud ->
            2

        CausticGasCloud ->
            3

        ParalyticGasCloud ->
            1


gasLog : GasKind -> String
gasLog kind =
    case kind of
        ParalyticGasCloud ->
            "Paralytic gas seizes your muscles!"

        _ ->
            "Choking gas burns your lungs!"



-- FIRE -------------------------------------------------------------------------------------------


{-| A cell catches fire (floor or grass only — never water, walls or chasm). -}
flammable : Pos -> Game -> Bool
flammable p game =
    case Level.at p game.level of
        Floor ->
            True

        Grass ->
            True

        _ ->
            False


{-| Ignite `center` and the flammable cells around it. -}
spawnFire : Pos -> Game -> Game
spawnFire center game =
    let
        cells =
            (center :: Grid.neighbors4 center)
                |> List.filter (\p -> flammable p game)

        lit =
            List.foldl (\p d -> Dict.insert ( p.x, p.y ) 4 d) game.fire cells
    in
    { game | fire = lit }


{-| Advance every fire: it spreads into adjacent tall grass, burns whoever stands in it, dies down by
one each turn, and leaves scorched floor where grass burns away. -}
tickFire : Game -> Game
tickFire game =
    if Dict.isEmpty game.fire then
        game

    else
        let
            burning =
                Dict.keys game.fire

            -- Tall grass next to a flame catches.
            ignited =
                burning
                    |> List.concatMap
                        (\( x, y ) ->
                            Grid.neighbors4 { x = x, y = y }
                                |> List.filter (\nb -> Level.at nb game.level == Grass && not (Dict.member ( nb.x, nb.y ) game.fire))
                        )

            withIgnited =
                List.foldl (\nb d -> Dict.insert ( nb.x, nb.y ) 4 d) game.fire ignited

            stepped =
                Dict.toList withIgnited |> List.map (\( k, t ) -> ( k, t - 1 ))

            ( alive, expired ) =
                List.partition (\( _, t ) -> t > 0) stepped

            -- Grass that finished burning becomes scorched floor.
            scorched =
                List.foldl
                    (\( ( x, y ), _ ) lv ->
                        if Level.at { x = x, y = y } lv == Grass then
                            Level.set { x = x, y = y } Floor lv

                        else
                            lv
                    )
                    game.level
                    expired
        in
        applyFireEffects { game | fire = Dict.fromList alive, level = scorched }


{-| Freeze the Water cells within one tile of `center` into ice patches (a timed overlay), dousing any
fire there. Ice is walkable (water already is) but visibly frozen, thawing back over several turns. -}
freezeWaterNear : Pos -> Game -> Game
freezeWaterNear center game =
    let
        frozen =
            cellsWithin 1 center
                |> List.filter (\p -> Level.at p game.level == Water)
    in
    if List.isEmpty frozen then
        game

    else
        { game
            | ice = List.foldl (\p d -> Dict.insert ( p.x, p.y ) 12 d) game.ice frozen
            , fire = List.foldl (\p d -> Dict.remove ( p.x, p.y ) d) game.fire frozen
        }


{-| Count down ice patches each turn, thawing those that reach zero back to open water. -}
tickIce : Game -> Game
tickIce game =
    if Dict.isEmpty game.ice then
        game

    else
        { game
            | ice =
                game.ice
                    |> Dict.toList
                    |> List.filterMap
                        (\( k, t ) ->
                            if t - 1 > 0 then
                                Just ( k, t - 1 )

                            else
                                Nothing
                        )
                    |> Dict.fromList
        }


{-| Burn the hero and any monster standing in fire this turn. -}
applyFireEffects : Game -> Game
applyFireEffects game =
    let
        afterHero =
            if Dict.member ( game.hero.pos.x, game.hero.pos.y ) game.fire then
                addStatus Burn 3 3 game |> addLog "Flames lick at you!"

            else
                game

        affected e =
            if Dict.member ( e.pos.x, e.pos.y ) afterHero.fire && e.def.id /= "fire-elemental" then
                { e | statuses = addEnemyStatus Burn 3 3 e.statuses, alerted = True }

            else
                e
    in
    { afterHero | enemies = List.map affected afterHero.enemies }


{-| Add or refresh a status on a monster (mirrors the hero's `addStatus`). -}
{-| Tick every monster's damage-over-time (burn/poison): apply the damage, count down the statuses,
remove any monster it kills (awarding XP and a popup). -}
tickEnemyStatuses : Game -> Game
tickEnemyStatuses game =
    let
        step e ( alive, killXp, killN, pops, logs ) =
            let
                dot =
                    e.statuses
                        |> List.filter (\s -> s.kind == Burn || s.kind == Poison || s.kind == Bleed)
                        |> List.map .magnitude
                        |> List.sum

                ticked =
                    e.statuses |> List.map (\s -> { s | turns = s.turns - 1 }) |> List.filter (\s -> s.turns > 0)

                newHp =
                    e.hp - dot
            in
            if dot > 0 && newHp <= 0 then
                ( alive
                , killXp + e.def.xp
                , killN + 1
                , { pos = e.pos, text = String.fromInt dot, color = "#ff7a3c" } :: pops
                , ("The " ++ e.def.name ++ " succumbs.") :: logs
                )

            else if dot > 0 then
                ( { e | hp = newHp, statuses = ticked } :: alive
                , killXp
                , killN
                , { pos = e.pos, text = String.fromInt dot, color = "#ff7a3c" } :: pops
                , logs
                )

            else
                ( { e | statuses = ticked } :: alive, killXp, killN, pops, logs )

        ( survivors, xp, n, popups, logs ) =
            List.foldl step ( [], 0, 0, [], [] ) game.enemies
    in
    { game
        | enemies = List.reverse survivors
        , kills = game.kills + n
        , popups = popups ++ game.popups
        , log = logs ++ game.log
    }
        |> gainXp xp


applyTimes : Int -> (a -> a) -> a -> a
applyTimes n f x =
    if n <= 0 then
        x

    else
        applyTimes (n - 1) f (f x)


{-| Burn one point of nutrition per turn; at zero the hero starves for 1 HP a turn. -}
tickHunger : Game -> Game
tickHunger game =
    let
        hero =
            game.hero

        drain =
            if List.member "starvation" game.challenges then
                2

            else
                1

        fed =
            hero.nutrition - drain
    in
    if fed <= 0 then
        checkHeroDeath ({ game | hero = { hero | hp = hero.hp - 1, nutrition = 0 } } |> addLog "You are starving!")

    else if fed >= maxNutrition * 3 // 4 && hero.hp < hero.maxHp && modBy 4 game.turn == 0 then
        -- Well-fed: a full belly knits wounds slowly between fights.
        { game | hero = { hero | nutrition = fed, hp = min hero.maxHp (hero.hp + 1) } }

    else
        { game | hero = { hero | nutrition = fed } }


{-| Each turn there's a chance the hero spots an adjacent secret door without searching, so one never
permanently blocks the way. -}
passivePerception : Game -> Game
passivePerception game =
    let
        ( notice, seed1 ) =
            Rng.chance 25 game.seed
    in
    if notice then
        let
            ( level1, found ) =
                revealSecretsNear (\p -> Grid.chebyshev p game.hero.pos == 1) game.level game.hero.pos
        in
        if found > 0 then
            { game | level = level1, seed = seed1 } |> addLog "You notice a hidden door!"

        else
            { game | seed = seed1 }

    else
        { game | seed = seed1 }


{-| Apply each active status to the hero (poison/burn drain HP, regen restores it, capped at max),
count it down, and drop the expired ones. Runs once per turn-consuming action. -}
tickStatuses : Game -> Game
tickStatuses game =
    let
        hero =
            game.hero

        -- Standing in water douses flames: the Burn status is quenched before it can tick.
        onWater =
            Level.at hero.pos game.level == Water

        wasBurning =
            onWater && List.any (\s -> s.kind == Burn) hero.statuses

        active =
            if onWater then
                List.filter (\s -> s.kind /= Burn) hero.statuses

            else
                hero.statuses

        ( hpDelta, logs0 ) =
            List.foldl
                (\status ( dhp, ls ) ->
                    case status.kind of
                        Regen ->
                            ( dhp + status.magnitude, ls )

                        Poison ->
                            ( dhp - status.magnitude, ("Poison gnaws at you (" ++ String.fromInt status.magnitude ++ ").") :: ls )

                        Burn ->
                            ( dhp - status.magnitude, ("Flames sear you (" ++ String.fromInt status.magnitude ++ ").") :: ls )

                        Bleed ->
                            ( dhp - status.magnitude, ("You bleed (" ++ String.fromInt status.magnitude ++ ").") :: ls )

                        _ ->
                            ( dhp, ls )
                )
                ( 0, [] )
                active

        logs =
            if wasBurning then
                "The water douses the flames." :: logs0

            else
                logs0

        ticked =
            active
                |> List.map (\s -> { s | turns = s.turns - 1 })
                |> List.filter (\s -> s.turns > 0)

        newHp =
            min hero.maxHp (hero.hp + hpDelta)
    in
    checkHeroDeath
        { game
            | hero = { hero | hp = newHp, statuses = ticked }
            , log = logs ++ game.log
        }


