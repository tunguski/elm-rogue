module Rogue.Game.Scene exposing (toScene)

{-| Projects a `Game` onto the renderer-agnostic `Rogue.Render.Scene` — the only thing a renderer
sees. Split out of `Rogue.Game`, which re-exposes `toScene` so importers are unchanged. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (ItemDef)
import Rogue.Game.Types exposing (..)
import Rogue.Game.Appearance exposing (..)
import Rogue.Grid exposing (Pos)
import Rogue.Level as Level exposing (Level)
import Rogue.Render as Render exposing (Scene)
import Rogue.Tile as Tile exposing (Tile(..))
import Set exposing (Set)




toScene : Game -> Scene
toScene game =
    { level = game.level
    , visible = game.visible
    , explored = game.explored
    , glyphs =
        List.map trapGlyph (List.filter .revealed game.traps)
            ++ plantGlyphs game.plants
            ++ wellGlyphs game.well
            ++ statueGlyphs game.statues
            ++ altarGlyphs game.altar
            ++ npcGlyphs game.npc
            ++ List.map chestGlyph game.chests
            ++ List.map shopGlyph game.shop
            ++ List.map (itemGlyph game.idents) game.items
            ++ List.map enemyGlyph game.enemies
            ++ [ heroGlyph game ]
    , popups = List.map (\p -> { pos = p.pos, text = p.text, color = p.color }) game.popups
    , gas =
        (game.gas
            |> Dict.toList
            |> List.filterMap
                (\( ( x, y ), g ) ->
                    if Set.member ( x, y ) game.visible then
                        Just { pos = { x = x, y = y }, color = gasColor g.kind, alpha = min 0.6 (0.12 + toFloat g.density * 0.08) }

                    else
                        Nothing
                )
        )
            ++ (game.fire
                    |> Dict.toList
                    |> List.filterMap
                        (\( ( x, y ), t ) ->
                            if Set.member ( x, y ) game.visible then
                                Just { pos = { x = x, y = y }, color = "#ff6a2a", alpha = min 0.7 (0.3 + toFloat t * 0.1) }

                            else
                                Nothing
                        )
               )
            ++ (game.ice
                    |> Dict.toList
                    |> List.filterMap
                        (\( ( x, y ), _ ) ->
                            if Set.member ( x, y ) game.visible then
                                Just { pos = { x = x, y = y }, color = "#bfe6ff", alpha = 0.34 }

                            else
                                Nothing
                        )
               )
    , healthBars =
        game.enemies
            |> List.filterMap
                (\e ->
                    if Set.member ( e.pos.x, e.pos.y ) game.visible && e.hp < e.def.maxHp then
                        Just { pos = e.pos, frac = toFloat (max 0 e.hp) / toFloat e.def.maxHp, ally = e.ally }

                    else
                        Nothing
                )
    , statusMarks =
        let
            marksFor pos statuses =
                case List.map (\s -> statusColor s.kind) statuses |> List.filter (\c -> c /= "") of
                    [] ->
                        Nothing

                    colors ->
                        Just { pos = pos, colors = List.take 4 colors }
        in
        (case marksFor game.hero.pos game.hero.statuses of
            Just m ->
                [ m ]

            Nothing ->
                []
        )
            ++ (game.enemies
                    |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
                    |> List.filterMap (\e -> marksFor e.pos e.statuses)
               )
    , mapMarkers =
        let
            explored p =
                Set.member ( p.x, p.y ) game.explored

            stairMarkers =
                (if explored game.stairsDown then
                    [ { pos = game.stairsDown, color = "#d8b24c" } ]

                 else
                    []
                )
                    ++ (if explored game.stairsUp then
                            [ { pos = game.stairsUp, color = "#4f8bff" } ]

                        else
                            []
                       )

            itemMarkers =
                game.items
                    |> List.filter (\it -> explored it.pos)
                    |> List.map (\it -> { pos = it.pos, color = "#ffe08a" })

            npcMarker =
                case game.npc of
                    Just n ->
                        if explored n.pos then
                            [ { pos = n.pos, color = "#6ad8c0" } ]

                        else
                            []

                    Nothing ->
                        []
        in
        stairMarkers ++ itemMarkers ++ npcMarker
    , theme = Render.themeForDepth game.depth
    , camera = game.hero.pos
    , cursor = Nothing
    , shake = False
    , pixelArt = True
    , time = 0
    , moves = []
    , stepStart = 0
    , torches = Level.torches game.level
    , hud =
        { title = "elm-rogue"
        , region = (Render.themeForDepth game.depth).name
        , level = game.hero.level
        , xp = game.hero.xp
        , xpNext = xpToNext game.hero.level
        , depth = game.depth
        , hp = game.hero.hp
        , maxHp = game.hero.maxHp
        , turn = game.turn
        , gold = game.hero.gold
        , hunger = hungerLabel game.hero.nutrition
        , weapon = equippedName game.hero.weapon (heroDamage game.hero) "dmg"
        , armour = equippedName game.hero.armour (heroDefense game.hero) "def"
        , ring =
            case game.hero.ring of
                Just r ->
                    r.name

                Nothing ->
                    ""
        , ability =
            if game.hero.heroClass == "" then
                ""

            else if game.hero.abilityCharge >= abilityMax then
                abilityName game.hero.heroClass ++ " — READY (Q)"

            else
                abilityName game.hero.heroClass ++ " (" ++ String.fromInt game.hero.abilityCharge ++ "/" ++ String.fromInt abilityMax ++ ")"
        , statuses = List.map statusLabel game.hero.statuses
        , keyring = keyringLabel game.hero.inventory
        , boss =
            game.enemies
                |> List.filter (\e -> e.def.boss && not e.ally)
                |> List.head
                |> Maybe.map (\e -> e.def.name)
                |> Maybe.withDefault ""
        , score =
            let
                base =
                    (game.depth * 120)
                        + (game.kills * 15)
                        + (if game.won then
                            1500

                           else
                            0
                          )
                        - (game.turn // 20)

                -- Each active challenge modifier is worth a +20% score bonus.
                multiplier =
                    100 + 20 * List.length game.challenges
            in
            max 0 (base * multiplier // 100)
        , inventory = List.map (displayName game.idents) game.hero.inventory
        , log = List.take 7 game.log
        , gameOver = game.gameOver
        , won = game.won
        , status = statusLine game
        }
    }


hungerLabel : Int -> String
hungerLabel nutrition =
    if nutrition > 300 then
        ""

    else if nutrition > 100 then
        "Hungry"

    else if nutrition > 0 then
        "Famished"

    else
        "Starving"


statusLine : Game -> String
statusLine game =
    if game.won then
        "Victory! Press R to play again"

    else if game.gameOver then
        "You have died at depth " ++ String.fromInt game.depth ++ " — press R to restart"

    else if game.ascending then
        "ASCENDING — flee to the surface! (climb up-stairs with >)"

    else if Level.at game.hero.pos game.level == StairsDown then
        "Press > to descend"

    else
        String.fromInt (List.length game.enemies) ++ " monsters · " ++ String.fromInt game.kills ++ " slain"


heroGlyph : Game -> Render.Glyph
heroGlyph game =
    { pos = game.hero.pos
    , char = game.hero.glyph
    , color = game.hero.color
    , layer = Render.layerHero
    , heavy = True
    , sprite =
        if game.hero.heroClass == "" then
            "hero"

        else
            "hero-" ++ game.hero.heroClass
    , tint = ""
    }


enemyGlyph : Enemy -> Render.Glyph
enemyGlyph enemy =
    { pos = enemy.pos
    , char = enemy.def.glyph
    , color =
        if enemy.ally then
            "#5dd47a"

        else
            enemy.def.color
    , layer = Render.layerActor
    , heavy = enemy.ally
    , sprite = enemy.def.id
    , tint =
        if enemy.ally then
            "#3ad17a"

        else
            ""
    }


{-| A HUD label for an equipped slot: the item's name (with enchant level, or a dash) plus the
resulting total stat. -}
equippedName : Maybe ItemDef -> Int -> String -> String
equippedName maybeItem total label =
    let
        prefix =
            case maybeItem of
                Just item ->
                    case item.kind of
                        Content.Equipment _ bonus ->
                            if bonus.plus > 0 then
                                item.name ++ " +" ++ String.fromInt bonus.plus

                            else
                                item.name

                        _ ->
                            item.name

                Nothing ->
                    "—"
    in
    prefix ++ " (" ++ label ++ " " ++ String.fromInt total ++ ")"


itemGlyph : Idents -> ItemOnFloor -> Render.Glyph
itemGlyph idents item =
    { pos = item.pos
    , char = item.def.glyph
    , color = displayColor idents item.def
    , layer = Render.layerItem
    , heavy = False
    , sprite = item.def.id
    , tint = ""
    }


npcGlyphs : Maybe Npc -> List Render.Glyph
npcGlyphs maybeNpc =
    case maybeNpc of
        Just n ->
            let
                ( ch, color ) =
                    case n.kind of
                        Ghost ->
                            ( "&", "#a9d6ff" )

                        Sage ->
                            ( "&", "#d6c27a" )

                        Wandmaker ->
                            ( "&", "#9be0ff" )

                        Blacksmith ->
                            ( "&", "#e0884b" )

                        Imp ->
                            ( "&", "#c97fe0" )
            in
            [ { pos = n.pos, char = ch, color = color, layer = Render.layerActor, heavy = True, sprite = "npc", tint = "" } ]

        Nothing ->
            []


statueGlyphs : List Pos -> List Render.Glyph
statueGlyphs statues =
    List.map
        (\p -> { pos = p, char = "&", color = "#7a7a82", layer = Render.layerItem, heavy = False, sprite = "statue", tint = "" })
        statues


wellGlyphs : Maybe Well -> List Render.Glyph
wellGlyphs maybeWell =
    case maybeWell of
        Just w ->
            let
                color =
                    case w.kind of
                        HealthWell ->
                            "#5dd47a"

                        AwarenessWell ->
                            "#82aaff"

                        TransmuteWell ->
                            "#6ad8c0"
            in
            [ { pos = w.pos, char = "○", color = color, layer = Render.layerItem, heavy = False, sprite = "well", tint = "" } ]

        Nothing ->
            []


plantGlyphs : Dict ( Int, Int ) PlantKind -> List Render.Glyph
plantGlyphs plants =
    plants
        |> Dict.toList
        |> List.map
            (\( ( x, y ), kind ) ->
                let
                    color =
                        case kind of
                            Firebloom ->
                                "#ff7a3c"

                            Sungrass ->
                                "#e0d24b"

                            Sorrowmoss ->
                                "#9b6ad8"

                            Earthroot ->
                                "#8a6a4a"
                in
                { pos = { x = x, y = y }, char = "♣", color = color, layer = Render.layerItem, heavy = False, sprite = "plant", tint = "" }
            )


chestGlyph : Chest -> Render.Glyph
chestGlyph chest =
    { pos = chest.pos
    , char = "0"
    , color =
        if chest.crystal then
            "#7fe0e0"

        else
            "#caa24a"
    , layer = Render.layerItem
    , heavy = True
    , sprite = "chest"
    , tint =
        if chest.crystal then
            "#7fe0e0"

        else
            ""
    }


altarGlyphs : Maybe Pos -> List Render.Glyph
altarGlyphs maybeAltar =
    case maybeAltar of
        Just p ->
            [ { pos = p, char = "_", color = "#9be0ff", layer = Render.layerItem, heavy = False, sprite = "altar", tint = "" } ]

        Nothing ->
            []


shopGlyph : ShopEntry -> Render.Glyph
shopGlyph entry =
    { pos = entry.pos
    , char = entry.def.glyph
    , color = "#ffd166"
    , layer = Render.layerItem
    , heavy = False
    , sprite = entry.def.id
    , tint = ""
    }


trapGlyph : Trap -> Render.Glyph
trapGlyph trap =
    { pos = trap.pos
    , char = "^"
    , color = "#e0824b"
    , layer = Render.layerTerrain
    , heavy = False
    , sprite = "trap"
    , tint = ""
    }
