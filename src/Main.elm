module Main exposing (main)

{-| The app shell: a class-selection screen (with a persistent run-history table), then the game,
drawn through a selectable `Rogue.Render.Renderer`. A toolbar switches the active **mod** (`Ruleset`)
and **rendering engine**; nothing in `Rogue.Game` knows which is chosen — the point of the seams.

Finished runs (death or victory) are persisted to `localStorage` via `Storage` and reloaded on start,
so your last few delves survive a page reload.

Controls: arrows / WASD / HJKL move, Y U B N diagonals, `.` wait, `Z` search, `>` descend, `1`-`9`
use/equip an item, `R` restart.
-}

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Mod.Default
import Mod.Hard
import Rogue.Content as Content exposing (ClassDef, Ruleset)
import Rogue.Game as Game exposing (Game)
import Rogue.Grid as Grid
import Rogue.Render exposing (Renderer)
import Rogue.Render.Ascii as AsciiRenderer
import Rogue.Render.Svg as SvgRenderer
import Rogue.Render.Webgl as WebglRenderer
import Set exposing (Set)
import Storage


type Screen
    = ClassSelect
    | Playing


{-| A pending mid-run pick that pauses play with a modal: a subclass (at the depth threshold) or a
talent (on level-up). -}
type Choice
    = SubclassChoice
    | TalentChoice
    | GhostChoice
    | EnchantChoice


type alias Model =
    { game : Game
    , screen : Screen
    , modName : String
    , rendererName : String
    , currentClass : String
    , history : List Run
    , showSheet : Bool
    , showBestiary : Bool
    , bestiary : Set String
    , targeting : Maybe Grid.Pos
    , damaged : Bool
    , pendingChoice : Maybe Choice
    , badges : Set String
    , challenges : Set String
    , remains : Maybe { depth : Int, gold : Int, itemId : String }
    , reduceMotion : Bool
    , showHints : Bool
    , pixelArt : Bool
    , time : Float
    , resumeSave : Maybe ( String, Game.SaveData )
    , seedInput : String
    , seedBump : Int
    }


{-| A persisted record of one finished delve. -}
type alias Run =
    { className : String
    , depth : Int
    , kills : Int
    , turns : Int
    , won : Bool
    }


type Msg
    = GameMsg Game.Msg
    | SelectMod String
    | SelectRenderer String
    | StartGame ClassDef
    | ToggleSheet
    | ToggleBestiary
    | SetSeed String
    | ToggleChallenge String
    | ToggleMotion
    | ToggleHints
    | TogglePixels
    | Tick Float
    | KeyPressed String
    | Continue
    | Choose String
    | Loaded (Maybe String)
    | LoadedSave (Maybe String)
    | LoadedBadges (Maybe String)
    | LoadedRemains (Maybe String)


dailySeed : Int
dailySeed =
    20260610


startSeed : Int
startSeed =
    20260610


storageKey : String
storageKey =
    "elm-rogue-history"


saveKey : String
saveKey =
    "elm-rogue-save"


badgesKey : String
badgesKey =
    "elm-rogue-badges"


remainsKey : String
remainsKey =
    "elm-rogue-remains"


maxHistory : Int
maxHistory =
    8


mods : List ( String, Ruleset )
mods =
    [ ( "Default", Mod.Default.ruleset )
    , ( "Hardcore", Mod.Hard.ruleset )
    ]


renderers : List ( String, Renderer Msg )
renderers =
    [ ( "SVG", SvgRenderer.renderer )
    , ( "ASCII", AsciiRenderer.renderer )
    , ( "3D", WebglRenderer.renderer )
    ]


lookup : String -> List ( String, a ) -> Maybe a
lookup name pairs =
    case pairs of
        [] ->
            Nothing

        ( n, v ) :: rest ->
            if n == name then
                Just v

            else
                lookup name rest


rulesetNamed : String -> Ruleset
rulesetNamed name =
    lookup name mods |> Maybe.withDefault Mod.Default.ruleset


rendererNamed : String -> Renderer Msg
rendererNamed name =
    lookup name renderers |> Maybe.withDefault SvgRenderer.renderer


{-| The depth at which the hero is offered a subclass. -}
subclassDepth : Int
subclassDepth =
    3


{-| Talents the hero hasn't learned and whose tier their level has unlocked (offered on level-up). -}
unlearnedTalents : Game -> List ( String, String )
unlearnedTalents game =
    Game.talentChoices
        |> List.filter (\t -> game.hero.level >= t.minLevel && not (List.member t.name game.hero.talents))
        |> List.map (\t -> ( t.name, t.desc ))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.newGame Mod.Default.ruleset (Content.defaultClass Mod.Default.ruleset) startSeed
      , screen = ClassSelect
      , modName = "Default"
      , rendererName = "SVG"
      , currentClass = "Adventurer"
      , history = []
      , showSheet = False
      , showBestiary = False
      , bestiary = Set.empty
      , targeting = Nothing
      , damaged = False
      , pendingChoice = Nothing
      , badges = Set.empty
      , challenges = Set.empty
      , remains = Nothing
      , reduceMotion = False
      , pixelArt = True
      , time = 0
      , showHints = True
      , resumeSave = Nothing
      , seedInput = ""
      , seedBump = 0
      }
    , Cmd.batch
        [ Storage.load storageKey Loaded
        , Storage.load saveKey LoadedSave
        , Storage.load badgesKey LoadedBadges
        , Storage.load remainsKey LoadedRemains
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded stored ->
            ( { model | history = parseHistory stored }, Cmd.none )

        LoadedSave stored ->
            ( { model | resumeSave = decodeSave stored }, Cmd.none )

        LoadedBadges stored ->
            ( { model | badges = stored |> Maybe.withDefault "" |> String.split "," |> List.filter (\x -> x /= "") |> Set.fromList }, Cmd.none )

        LoadedRemains stored ->
            ( { model | remains = decodeRemains stored }, Cmd.none )

        Continue ->
            case model.resumeSave of
                Just ( savedMod, save ) ->
                    ( { model
                        | game = Game.resume (rulesetNamed savedMod) save
                        , modName = savedMod
                        , currentClass = "Wanderer"
                        , bestiary = Set.empty
                        , screen = Playing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ToggleSheet ->
            ( { model | showSheet = not model.showSheet }, Cmd.none )

        ToggleBestiary ->
            ( { model | showBestiary = not model.showBestiary }, Cmd.none )

        SetSeed text ->
            ( { model | seedInput = text }, Cmd.none )

        ToggleMotion ->
            ( { model | reduceMotion = not model.reduceMotion }, Cmd.none )

        ToggleHints ->
            ( { model | showHints = not model.showHints }, Cmd.none )

        TogglePixels ->
            ( { model | pixelArt = not model.pixelArt }, Cmd.none )

        Tick dt ->
            -- Accumulate wall-clock time (ms) for the 3D renderer's continuous animations.
            ( { model | time = model.time + dt }, Cmd.none )

        ToggleChallenge id ->
            ( { model
                | challenges =
                    if Set.member id model.challenges then
                        Set.remove id model.challenges

                    else
                        Set.insert id model.challenges
              }
            , Cmd.none
            )

        KeyPressed key ->
            if model.pendingChoice /= Nothing then
                ( model, Cmd.none )

            else
                handleKey (String.toLower key) model

        Choose name ->
            case model.pendingChoice of
                Just SubclassChoice ->
                    ( { model | game = Game.chooseSubclass name model.game, pendingChoice = Nothing }, Cmd.none )

                Just TalentChoice ->
                    ( { model | game = Game.learnTalent name model.game, pendingChoice = Nothing }, Cmd.none )

                Just GhostChoice ->
                    ( { model | game = Game.takeGhostGift name model.game, pendingChoice = Nothing }, Cmd.none )

                Just EnchantChoice ->
                    ( { model | game = Game.chooseEnchant name model.game, pendingChoice = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectRenderer name ->
            ( { model | rendererName = name }, Cmd.none )

        SelectMod name ->
            ( { model | modName = name, screen = ClassSelect }, Cmd.none )

        StartGame class ->
            let
                withChallenges =
                    Game.newGame (rulesetNamed model.modName) class (chosenSeed model)
                        |> Game.startChallenges (Set.toList model.challenges)

                started =
                    case model.remains of
                        Just r ->
                            Game.setRemains r withChallenges

                        Nothing ->
                            withChallenges
            in
            ( { model
                | game = started
                , currentClass = class.name
                , bestiary = Set.empty
                , remains = Nothing
                , screen = Playing
              }
            , Storage.save remainsKey ""
            )

        GameMsg gm ->
            runGame gm model


{-| Apply a game message: handle restart (back to class select) or step the engine, folding the seen
monsters into the bestiary and persisting a finished run. Shared by `GameMsg` and key handling. -}
runGame : Game.Msg -> Model -> ( Model, Cmd Msg )
runGame gm model =
    case gm of
        Game.Restart ->
            ( { model
                | screen = ClassSelect
                , seedBump = model.seedBump + model.game.turn + model.game.depth * 1009 + 1
              }
            , Cmd.none
            )

        _ ->
            let
                nextGame =
                    Game.update gm model.game

                justEnded =
                    nextGame.gameOver && not model.game.gameOver

                bestiary =
                    Set.union model.bestiary (seenMonsters nextGame)

                pending =
                    if nextGame.awaitingGhostGift && not model.game.awaitingGhostGift then
                        Just GhostChoice

                    else if nextGame.awaitingEnchant && not model.game.awaitingEnchant then
                        Just EnchantChoice

                    else if nextGame.hero.subclass == Nothing && nextGame.depth >= subclassDepth then
                        Just SubclassChoice

                    else if nextGame.hero.level > model.game.hero.level && not (List.isEmpty (unlearnedTalents nextGame)) then
                        Just TalentChoice

                    else
                        model.pendingChoice

                base =
                    { model
                        | game = nextGame
                        , bestiary = bestiary
                        , damaged = nextGame.hero.hp < model.game.hero.hp
                        , pendingChoice = pending
                    }
            in
            if justEnded then
                let
                    run =
                        { className = model.currentClass
                        , depth = nextGame.depth
                        , kills = nextGame.kills
                        , turns = nextGame.turn
                        , won = nextGame.won
                        }

                    history =
                        List.take maxHistory (run :: model.history)

                    newBadges =
                        Set.union model.badges (badgesFromRun run)

                    -- Leave remains (gold + first carried item) only on death, to find next run.
                    remainsCmd =
                        if nextGame.won then
                            Storage.save remainsKey ""

                        else
                            Storage.save remainsKey
                                (String.join "|"
                                    [ String.fromInt nextGame.depth
                                    , String.fromInt nextGame.hero.gold
                                    , List.head nextGame.hero.inventory |> Maybe.map .id |> Maybe.withDefault ""
                                    ]
                                )
                in
                ( { base | history = history, badges = newBadges, resumeSave = Nothing }
                , Cmd.batch
                    [ Storage.save storageKey (encodeHistory history)
                    , Storage.save saveKey ""
                    , Storage.save badgesKey (String.join "," (Set.toList newBadges))
                    , remainsCmd
                    ]
                )

            else
                ( base, Storage.save saveKey (encodeSave model.modName nextGame) )


{-| Route a keypress: overlays and the targeting cursor capture keys first; otherwise it's a normal
play key. Targeting (entered with `f`) cycles monsters with `Tab`, throws with `f`/Enter, cancels with
anything else. -}
handleKey : String -> Model -> ( Model, Cmd Msg )
handleKey key model =
    case model.targeting of
        Just target ->
            if key == "f" || key == "enter" || key == " " then
                runGame (Game.ThrowAt target) { model | targeting = Nothing }

            else if key == "tab" then
                ( { model | targeting = nextTarget model.game target }, Cmd.none )

            else
                ( { model | targeting = Nothing }, Cmd.none )

        Nothing ->
            case key of
                "i" ->
                    ( { model | showSheet = not model.showSheet }, Cmd.none )

                "m" ->
                    ( { model | showBestiary = not model.showBestiary }, Cmd.none )

                "f" ->
                    case nearestTarget model.game of
                        Just t ->
                            ( { model | targeting = Just t }, Cmd.none )

                        Nothing ->
                            runGame Game.Fire model

                _ ->
                    runGame (keyToGameMsg key) model


{-| The nearest visible monster to the hero, if any (the initial target). -}
nearestTarget : Game -> Maybe Grid.Pos
nearestTarget game =
    game.enemies
        |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
        |> List.foldl
            (\e best ->
                case best of
                    Nothing ->
                        Just e.pos

                    Just b ->
                        if Grid.chebyshev e.pos game.hero.pos < Grid.chebyshev b game.hero.pos then
                            Just e.pos

                        else
                            best
            )
            Nothing


{-| Cycle to the next visible monster after `current` (wraps), for Tab-targeting. -}
nextTarget : Game -> Grid.Pos -> Maybe Grid.Pos
nextTarget game current =
    let
        visible =
            game.enemies
                |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
                |> List.map .pos

        after =
            List.filter (\p -> p /= current) visible
    in
    case after of
        next :: _ ->
            Just next

        [] ->
            Just current



-- HISTORY PERSISTENCE ----------------------------------------------------------------------------


{-| Records are stored one per line as `class|depth|kills|turns|W|D` — a tiny delimited format, so no
JSON codecs are needed for this small amount of state. -}
encodeHistory : List Run -> String
encodeHistory runs =
    String.join "\n" (List.map encodeRun runs)


encodeRun : Run -> String
encodeRun run =
    String.join "|"
        [ run.className
        , String.fromInt run.depth
        , String.fromInt run.kills
        , String.fromInt run.turns
        , if run.won then
            "W"

          else
            "D"
        ]


parseHistory : Maybe String -> List Run
parseHistory stored =
    case stored of
        Nothing ->
            []

        Just text ->
            text
                |> String.split "\n"
                |> List.filterMap parseRun


{-| The in-progress run, as a single delimited line: `mod|depth|hp|maxHp|dmg|def|gold|lvl|xp|nut|fov|
glyph|color|weapon|armour|ring|inv,ids|known,ids|seed`. Gear/inventory are stored by item id and
re-resolved against the ruleset on load. -}
encodeSave : String -> Game -> String
encodeSave modName game =
    let
        hero =
            game.hero

        idOr dash maybe =
            Maybe.map .id maybe |> Maybe.withDefault dash
    in
    String.join "|"
        [ modName
        , String.fromInt game.depth
        , String.fromInt hero.hp
        , String.fromInt hero.maxHp
        , String.fromInt hero.damage
        , String.fromInt hero.defense
        , String.fromInt hero.gold
        , String.fromInt hero.level
        , String.fromInt hero.xp
        , String.fromInt hero.nutrition
        , String.fromInt hero.fovRadius
        , hero.glyph
        , hero.color
        , idOr "-" hero.weapon
        , idOr "-" hero.armour
        , idOr "-" hero.ring
        , String.join "," (List.map .id hero.inventory)
        , String.join "," (Game.knownItemIds game)
        , String.fromInt (game.turn + game.depth * 1009 + 777)
        ]


decodeSave : Maybe String -> Maybe ( String, Game.SaveData )
decodeSave stored =
    case stored of
        Just text ->
            case String.split "|" text of
                [ m, d, hp, mhp, dmg, df, gold, lvl, xp, nut, fov, glyph, color, w, a, r, inv, known, sd ] ->
                    let
                        int s =
                            String.toInt s |> Maybe.withDefault 0

                        optId s =
                            if s == "-" || s == "" then
                                Nothing

                            else
                                Just s

                        ids s =
                            String.split "," s |> List.filter (\x -> x /= "")
                    in
                    Just
                        ( m
                        , { depth = int d
                          , hp = int hp
                          , maxHp = int mhp
                          , damage = int dmg
                          , defense = int df
                          , gold = int gold
                          , level = int lvl
                          , xp = int xp
                          , nutrition = int nut
                          , fovRadius = int fov
                          , glyph = glyph
                          , color = color
                          , weaponId = optId w
                          , armourId = optId a
                          , ringId = optId r
                          , inventoryIds = ids inv
                          , knownIds = ids known
                          , seed = int sd
                          }
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


{-| Decode the persisted remains line `depth|gold|itemId` (empty / malformed → none). -}
decodeRemains : Maybe String -> Maybe { depth : Int, gold : Int, itemId : String }
decodeRemains stored =
    case Maybe.map (String.split "|") stored of
        Just [ d, g, itemId ] ->
            Maybe.map2 (\depth gold -> { depth = depth, gold = gold, itemId = itemId })
                (String.toInt d)
                (String.toInt g)

        _ ->
            Nothing


parseRun : String -> Maybe Run
parseRun line =
    case String.split "|" line of
        [ name, d, k, t, result ] ->
            Maybe.map3
                (\depth kills turns ->
                    { className = name, depth = depth, kills = kills, turns = turns, won = result == "W" }
                )
                (String.toInt d)
                (String.toInt k)
                (String.toInt t)

        _ ->
            Nothing



-- VIEW -------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class
            (if model.reduceMotion then
                "rg-root rg-reduce-motion"

             else
                "rg-root"
            )
        ]
        [ toolbar model
        , case model.screen of
            ClassSelect ->
                classSelectView model

            Playing ->
                Html.div []
                    [ Html.div [ HA.class "rg-keyhint" ]
                        [ Html.text "move: WASD/HJKL · diag: Y U B N · wait: . · search: Z · aim: F · ability: Q · examine: X · explore: O · brew: C · inventory: I · bestiary: M · descend: > · use: 1-9 · restart: R" ]
                    , hintBar model
                    , (rendererNamed model.rendererName).view (sceneFor model)
                    , quickslotBar model
                    , touchControls model
                    , if model.showSheet then
                        sheetView model.game

                      else
                        Html.text ""
                    , if model.showBestiary then
                        bestiaryView model

                      else
                        Html.text ""
                    , choiceOverlay model
                    ]
        ]


{-| A modal that pauses play to pick a subclass (at the depth threshold) or a talent (on level-up). -}
choiceOverlay : Model -> Html Msg
choiceOverlay model =
    case model.pendingChoice of
        Nothing ->
            Html.text ""

        Just choice ->
            let
                ( title, options ) =
                    case choice of
                        SubclassChoice ->
                            ( "Choose your path", Game.subclassChoices )

                        TalentChoice ->
                            ( "Level up — learn a talent", unlearnedTalents model.game )

                        GhostChoice ->
                            let
                                bladeName =
                                    model.game.quest |> Maybe.map (\q -> q.reward.name) |> Maybe.withDefault "an old blade"
                            in
                            ( "The sad ghost's gift"
                            , [ ( "rose", "Dried Rose — a wilted artifact that blossoms into a ghostly ally" )
                              , ( "blade", bladeName ++ " — the ghost's own weapon" )
                              ]
                            )

                        EnchantChoice ->
                            ( "Choose a weapon enchantment"
                            , [ ( "blazing", "Blazing — sets struck foes alight" )
                              , ( "vampiric", "Vampiric — heals you for a share of the damage dealt" )
                              , ( "grim", "Grim — a chance to slay wounded foes outright" )
                              ]
                            )
            in
            Html.div [ HA.class "rg-overlay" ]
                [ Html.div [ HA.class "rg-modal" ]
                    (Html.div [ HA.class "rg-modal-title" ] [ Html.text title ]
                        :: List.map choiceCard options
                    )
                ]


choiceCard : ( String, String ) -> Html Msg
choiceCard ( name, desc ) =
    Html.button [ onClick (Choose name), HA.class "rg-btn rg-card", HA.style "width" "100%", HA.style "margin-bottom" "6px" ]
        [ Html.div [ HA.class "rg-card-name" ] [ Html.text name ]
        , Html.div [ HA.class "rg-card-stats" ] [ Html.text desc ]
        ]


bestiaryView : Model -> Html Msg
bestiaryView model =
    let
        roster =
            (rulesetNamed model.modName).enemies ++ (rulesetNamed model.modName).bosses
    in
    Html.div [ HA.class "rg-overlay" ]
        [ Html.div [ HA.class "rg-modal" ]
            (Html.div [ HA.class "rg-modal-title" ] [ Html.text "Bestiary" ]
                :: List.map (bestiaryRow model.bestiary) roster
                ++ (Html.div [ HA.class "rg-modal-title" ] [ Html.text "Catalog" ]
                        :: catalogRows model.game
                   )
                ++ [ Html.button [ onClick ToggleBestiary, HA.class "rg-modal-close" ]
                        [ Html.text "Close (M)" ]
                   ]
            )
        ]


{-| The item discovery journal: every item type grouped by category, identified ones shown by their
true name and undiscovered consumables masked behind their per-run appearance. -}
catalogRows : Game -> List (Html Msg)
catalogRows game =
    let
        entries =
            Game.itemCatalog game

        categories =
            [ "Weapons", "Armour", "Potions", "Scrolls", "Wands", "Rings", "Artifacts", "Other" ]

        row e =
            Html.div [ HA.class "rg-bestiary-row" ]
                [ Html.span [ HA.style "color" e.color ] [ Html.text (e.glyph ++ "  " ++ e.name) ]
                , Html.span [ HA.class "rg-item-desc" ]
                    [ Html.text
                        (if e.known then
                            "✓"

                         else
                            "?"
                        )
                    ]
                ]

        section cat =
            case List.filter (\e -> e.category == cat) entries of
                [] ->
                    []

                rows ->
                    Html.div [ HA.class "rg-inv-hint" ] [ Html.text cat ] :: List.map row rows
    in
    List.concatMap section categories


bestiaryRow : Set String -> Content.EnemyDef -> Html Msg
bestiaryRow seen def =
    if Set.member def.id seen then
        Html.div [ HA.class "rg-bestiary-row" ]
            [ Html.span [ HA.style "color" def.color ] [ Html.text (def.glyph ++ "  " ++ def.name) ]
            , Html.span [ HA.class "rg-item-desc" ]
                [ Html.text ("HP " ++ String.fromInt def.maxHp ++ " · DMG " ++ String.fromInt def.damage ++ " · DEF " ++ String.fromInt def.defense ++ " · " ++ String.fromInt def.xp ++ "xp") ]
            , Html.div [ HA.class "rg-item-desc", HA.style "width" "100%", HA.style "font-style" "italic", HA.style "opacity" "0.8" ]
                [ Html.text (bestiaryLore def.id) ]
            ]

    else
        Html.div [ HA.class "rg-bestiary-row", HA.style "color" "#3f4b5e" ]
            [ Html.text "????  — undiscovered" ]


{-| A short, original flavour line for each monster, shown once it's been encountered. -}
bestiaryLore : String -> String
bestiaryLore id =
    case id of
        "rat" ->
            "A scrawny sewer rat, bold only in numbers."

        "marsupial-rat" ->
            "A pouched cousin of the sewer rat, quicker and meaner."

        "gnoll-scout" ->
            "A wiry skirmisher that ranges ahead of the warband."

        "gnoll-archer" ->
            "Keeps its distance and looses crude arrows from the dark."

        "crab" ->
            "An armour-shelled scavenger; its claws crack bone."

        "skeleton" ->
            "Animated bones that clatter back up unless truly shattered."

        "swarm" ->
            "A churning cloud of biting flies that splits when struck."

        "slime" ->
            "A corrosive ooze that digests anything it engulfs."

        "thief" ->
            "A nimble cutpurse that grabs your gold and bolts."

        "gnoll-brute" ->
            "A slab of muscle and scars that hits like a falling wall."

        "gnoll-shaman" ->
            "Channels crude spirit-magic to mend and madden its kin."

        "prison-guard" ->
            "A disciplined jailer in heavy mail, slow but unyielding."

        "cave-bat" ->
            "A leathery flier that darts erratically for the throat."

        "piranha" ->
            "A frenzy of teeth that never leaves the water."

        "stone-golem" ->
            "A lumbering construct of living rock, near impervious."

        "spore-fungus" ->
            "A bloated growth that coughs choking spores into the air around it."

        "ghoul" ->
            "A loathsome corpse-eater that drags itself back up once felled."

        "fire-elemental" ->
            "A whirling pillar of living flame; fire only feeds it."

        "warlock" ->
            "A hooded hexer whose curses wither flesh from afar."

        "necromancer" ->
            "Calls the restless dead to swell its grim ranks."

        "dwarf-monk" ->
            "A bare-fisted ascetic whose strikes shatter guard and bone."

        "demon" ->
            "A horned fiend wreathed in heat, hungry for souls."

        "succubus" ->
            "A beguiling terror that drains the will of the smitten."

        "sheep" ->
            "Harmless and bewildered — once something far worse."

        "gnoll-warlord" ->
            "The warband's iron-fisted chieftain, enraged by blood."

        "spider-queen" ->
            "A bloated matriarch trailing broods of venomous young."

        "dwarf-king" ->
            "The last crowned ruler of the buried city, raised in undeath."

        "yog-dzewa" ->
            "An ancient horror dreaming at the dungeon's rotten heart."

        _ ->
            "A denizen of the deep dungeon."


sheetView : Game -> Html Msg
sheetView game =
    let
        hero =
            game.hero
    in
    Html.div [ HA.class "rg-overlay" ]
        [ Html.div [ HA.class "rg-modal" ]
            [ Html.div [ HA.class "rg-modal-title" ]
                [ Html.text "Character" ]
            , sheetStat "Level" (String.fromInt hero.level)
            , sheetStat "HP" (String.fromInt (max 0 hero.hp) ++ " / " ++ String.fromInt hero.maxHp)
            , sheetStat "Gold" (String.fromInt hero.gold)
            , Html.div [ HA.class "rg-inv-hint" ]
                [ Html.text
                    (if Game.nearShop game then
                        "Inventory — at a shop (tap to use · 💰 to sell · ✕ to drop)"

                     else
                        "Inventory — by bag (tap to use · ✕ to drop)"
                    )
                ]
            , if List.isEmpty hero.inventory then
                Html.div [ HA.class "rg-inv-empty" ] [ Html.text "— empty —" ]

              else
                Html.div [] (categorizedInventory (Game.nearShop game) hero.inventory)
            , Html.div [ HA.class "rg-inv-hint" ] [ Html.text "Alchemy recipes (brew with C)" ]
            , Html.div []
                (List.map
                    (\r ->
                        Html.div [ HA.style "font-size" "11.5px", HA.style "color" "#7f8ba0", HA.style "padding" "1px 0" ]
                            [ Html.text (String.join " + " r.inputs ++ " → " ++ r.name) ]
                    )
                    Game.alchemyRecipes
                )
            , Html.button [ onClick ToggleSheet, HA.class "rg-modal-close" ]
                [ Html.text "Close (I)" ]
            ]
        ]


{-| The bag (category) an item belongs to, for the organised inventory display. -}
itemBag : Content.ItemDef -> String
itemBag def =
    case def.kind of
        Content.Wand _ ->
            "Wand holster"

        Content.Artifact _ ->
            "Artifacts"

        Content.Equipment _ _ ->
            "Gear"

        Content.Key ->
            "Keyring"

        Content.Consumable _ ->
            if String.startsWith "potion" def.id then
                "Potion bandolier"

            else if String.startsWith "scroll" def.id then
                "Scroll holder"

            else if List.member def.id [ "darts", "javelin", "shuriken", "bomb" ] then
                "Throwables"

            else
                "Pouch"


{-| The inventory grouped into labelled bags, each item keeping its true inventory index for use/drop. -}
categorizedInventory : Bool -> List Content.ItemDef -> List (Html Msg)
categorizedInventory shopping inventory =
    let
        indexed =
            List.indexedMap Tuple.pair inventory

        bags =
            [ "Potion bandolier", "Scroll holder", "Wand holster", "Artifacts", "Gear", "Throwables", "Keyring", "Pouch" ]

        section bag =
            let
                items =
                    List.filter (\( _, def ) -> itemBag def == bag) indexed
            in
            if List.isEmpty items then
                []

            else
                Html.div [ HA.style "color" "#5b6b82", HA.style "font-size" "11px", HA.style "margin" "6px 0 2px" ] [ Html.text bag ]
                    :: List.map (\( i, def ) -> sheetItem shopping i def) items
    in
    List.concatMap section bags


sheetStat : String -> String -> Html Msg
sheetStat label value =
    Html.div [ HA.class "rg-stat" ]
        [ Html.span [ HA.class "rg-stat-label" ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


sheetItem : Bool -> Int -> Content.ItemDef -> Html Msg
sheetItem shopping i def =
    Html.div [ HA.class "rg-item-row" ]
        ([ Html.button [ onClick (GameMsg (Game.Use i)), HA.class "rg-btn rg-item-use" ]
            [ Html.span [ HA.style "color" def.color ] [ Html.text (String.fromInt (i + 1) ++ ". " ++ def.name) ]
            , Html.span [ HA.class "rg-item-desc" ] [ Html.text (Content.describe def) ]
            ]
         ]
            ++ (if shopping then
                    [ Html.button [ onClick (GameMsg (Game.Sell i)), HA.class "rg-btn rg-item-drop" ] [ Html.text "💰" ] ]

                else
                    []
               )
            ++ [ Html.button [ onClick (GameMsg (Game.Drop i)), HA.class "rg-btn rg-item-drop" ]
                    [ Html.text "✕" ]
               ]
        )


{-| A hotbar of the first inventory items: a tappable quick-use button per item (numbered, matching the
1–9 keys), so items are usable without opening the sheet — essential on touch, handy on desktop. Labels
reuse the renderer's display names so unidentified items stay disguised. -}
quickslotBar : Model -> Html Msg
quickslotBar model =
    let
        names =
            (Game.toScene model.game).hud.inventory |> List.take 8
    in
    if List.isEmpty names then
        Html.text ""

    else
        Html.div [ HA.class "rg-quickbar" ]
            (List.indexedMap
                (\i name ->
                    Html.button [ onClick (GameMsg (Game.Use i)), HA.class "rg-btn rg-quick" ]
                        [ Html.span [ HA.class "rg-quick-num" ] [ Html.text (String.fromInt (i + 1)) ]
                        , Html.text name
                        ]
                )
                names
            )


{-| A single line of contextual coaching, shown only while the "Hints" setting is on (M122 toggle).

Pixel-Dungeon-style onboarding: rather than a wall of tutorial text, surface one timely tip for the
*current* situation — wounded, ability charged, carrying the Amulet, or just starting out. The most
urgent condition wins, and once the player is mid-run with nothing pressing the line disappears, so it
never nags. Everything it needs is already in the `Hud`, so the engine stays untouched. -}
hintBar : Model -> Html Msg
hintBar model =
    if not model.showHints then
        Html.text ""

    else
        case hintFor model of
            Nothing ->
                Html.text ""

            Just tip ->
                Html.div [ HA.class "rg-hint" ] [ Html.text ("💡 " ++ tip) ]


hintFor : Model -> Maybe String
hintFor model =
    let
        hud =
            (Game.toScene model.game).hud
    in
    if hud.gameOver then
        Nothing

    else if model.game.ascending then
        Just "You carry the Amulet — flee upward! Reach the surface stairs to win."

    else if hud.maxHp > 0 && hud.hp * 4 <= hud.maxHp then
        Just "Badly wounded — drink a healing potion (press its slot number) or retreat to safety."

    else if String.contains "READY" hud.ability then
        Just "Your class ability is charged — press Q to unleash it."

    else if hud.turn == 0 then
        Just "Bump into monsters to attack. Press O to auto-explore, X to examine, I for your sheet."

    else if hud.turn < 8 && List.isEmpty hud.inventory then
        Just "Step onto items to pick them up; descend with > once you find the stairs down."

    else
        Nothing


{-| On-screen touch controls (a directional pad plus action buttons) so the game is playable without a
keyboard. Hidden by CSS on non-touch wide screens; the buttons dispatch the same messages the keys do. -}
touchControls : Model -> Html Msg
touchControls model =
    let
        aiming =
            model.targeting /= Nothing
    in
    Html.div [ HA.class "rg-touch" ]
        [ Html.div [ HA.class "rg-padgrid" ]
            [ padBtn "↖" (GameMsg (Game.Move Grid.dirNW))
            , padBtn "↑" (GameMsg (Game.Move Grid.dirN))
            , padBtn "↗" (GameMsg (Game.Move Grid.dirNE))
            , padBtn "←" (GameMsg (Game.Move Grid.dirW))
            , padBtn "·" (GameMsg Game.Wait)
            , padBtn "→" (GameMsg (Game.Move Grid.dirE))
            , padBtn "↙" (GameMsg (Game.Move Grid.dirSW))
            , padBtn "↓" (GameMsg (Game.Move Grid.dirS))
            , padBtn "↘" (GameMsg (Game.Move Grid.dirSE))
            ]
        , Html.div [ HA.class "rg-actgrid" ]
            [ actBtn
                (if aiming then
                    "✦ Throw"

                 else
                    "✦ Aim"
                )
                (KeyPressed "f")
            , actBtn "↹ Next" (KeyPressed "tab")
            , actBtn "Ability" (GameMsg Game.Ability)
            , actBtn "Explore" (GameMsg Game.AutoExplore)
            , actBtn "Descend" (GameMsg Game.Descend)
            , actBtn "Search" (GameMsg Game.Search)
            , actBtn "Brew" (GameMsg Game.Brew)
            , actBtn "Items" ToggleSheet
            , actBtn "Beasts" ToggleBestiary
            , actBtn "Restart" (GameMsg Game.Restart)
            ]
        ]


padBtn : String -> Msg -> Html Msg
padBtn label msg =
    Html.button [ onClick msg, HA.class "rg-btn rg-pad" ] [ Html.text label ]


actBtn : String -> Msg -> Html Msg
actBtn label msg =
    Html.button [ onClick msg, HA.class "rg-btn rg-act" ] [ Html.text label ]


{-| The render scene for the current frame, with the targeting cursor overlaid when aiming. -}
sceneFor : Model -> Rogue.Render.Scene
sceneFor model =
    let
        scene =
            Game.toScene model.game
    in
    { scene | cursor = model.targeting, shake = model.damaged, pixelArt = model.pixelArt, time = model.time }


toolbar : Model -> Html Msg
toolbar model =
    Html.div [ HA.class "rg-toolbar" ]
        [ Html.div [ HA.class "rg-title" ] [ Html.text "elm-rogue" ]
        , chipGroup "Mod" (List.map Tuple.first mods) model.modName SelectMod
        , chipGroup "Renderer" (List.map Tuple.first renderers) model.rendererName SelectRenderer
        , Html.div [ HA.class "rg-chipgroup" ]
            [ Html.span [ HA.class "rg-chiplabel" ] [ Html.text "Settings" ]
            , chip "Reduce motion" model.reduceMotion ToggleMotion
            , chip "Hints" model.showHints ToggleHints
            , chip "Pixel art" model.pixelArt TogglePixels
            ]
        ]


chipGroup : String -> List String -> String -> (String -> Msg) -> Html Msg
chipGroup label names active toMsg =
    Html.div [ HA.class "rg-chipgroup" ]
        (Html.span [ HA.class "rg-chiplabel" ] [ Html.text label ]
            :: List.map (\name -> chip name (name == active) (toMsg name)) names
        )


chip : String -> Bool -> Msg -> Html Msg
chip label active msg =
    Html.button
        [ onClick msg
        , HA.class
            (if active then
                "rg-chip is-active"

             else
                "rg-chip"
            )
        ]
        [ Html.text label ]



-- CLASS SELECT -----------------------------------------------------------------------------------


chosenSeed : Model -> Int
chosenSeed model =
    case String.toInt (String.trim model.seedInput) of
        Just n ->
            n

        Nothing ->
            startSeed + model.seedBump


classSelectView : Model -> Html Msg
classSelectView model =
    Html.div [ HA.class "rg-classselect" ]
        [ Html.div [ HA.class "rg-subtitle" ]
            [ Html.text ("Choose your class — " ++ model.modName ++ " mod") ]
        , continueRow model
        , seedRow model
        , challengeRow model
        , Html.div [ HA.class "rg-cards" ]
            (List.map classCard (rulesetNamed model.modName).classes)
        , badgesView model.badges
        , historyView model.history
        ]


{-| A "Continue" banner offering to resume the saved in-progress run, if one exists. -}
continueRow : Model -> Html Msg
continueRow model =
    case model.resumeSave of
        Just ( savedMod, save ) ->
            Html.div [ HA.class "rg-continue-row" ]
                [ Html.button [ onClick Continue, HA.class "rg-continue-btn" ]
                    [ Html.text ("▶ Continue saved run — depth " ++ String.fromInt save.depth ++ " (" ++ savedMod ++ ")") ]
                ]

        Nothing ->
            Html.text ""


{-| Optional run-modifier challenge toggles, applied to the next game started. -}
challengeRow : Model -> Html Msg
challengeRow model =
    Html.div [ HA.class "rg-seedrow", HA.style "flex-wrap" "wrap" ]
        (Html.span [ HA.class "rg-seed-label" ] [ Html.text "Challenges" ]
            :: List.map
                (\( id, ( label, desc ) ) ->
                    chip (label ++ " — " ++ desc) (Set.member id model.challenges) (ToggleChallenge id)
                )
                Game.challengeChoices
        )


seedRow : Model -> Html Msg
seedRow model =
    Html.div [ HA.class "rg-seedrow" ]
        [ Html.span [ HA.class "rg-seed-label" ] [ Html.text "Seed" ]
        , Html.input
            [ HA.value model.seedInput
            , onInput SetSeed
            , HA.placeholder "random"
            , HA.class "rg-seed-input"
            ]
            []
        , chip ("Daily " ++ String.fromInt dailySeed) (String.trim model.seedInput == String.fromInt dailySeed) (SetSeed (String.fromInt dailySeed))
        , chip "Clear" (model.seedInput == "") (SetSeed "")
        ]


classCard : ClassDef -> Html Msg
classCard class =
    Html.button [ onClick (StartGame class), HA.class "rg-card" ]
        [ Html.div [ HA.class "rg-card-name", HA.style "color" class.color ]
            [ Html.text (class.glyph ++ "  " ++ class.name) ]
        , Html.div [ HA.class "rg-card-desc" ]
            [ Html.text class.description ]
        , Html.div [ HA.class "rg-card-stats" ]
            [ Html.text ("HP " ++ String.fromInt class.maxHp ++ " · DMG " ++ String.fromInt class.damage ++ " · DEF " ++ String.fromInt class.defense ++ " · FOV " ++ String.fromInt class.fovRadius) ]
        ]


{-| A run's score: rewards going deep and slaying much, with a big bonus for claiming the Amulet and a
small efficiency nudge against dawdling. Used for the rankings and the personal best. -}
scoreFor : Run -> Int
scoreFor run =
    (run.depth * 120)
        + (run.kills * 15)
        + (if run.won then
            1500

           else
            0
          )
        - (run.turns // 20)
        |> max 0


historyView : List Run -> Html Msg
historyView runs =
    if List.isEmpty runs then
        Html.text ""

    else
        let
            best =
                runs |> List.map scoreFor |> List.maximum |> Maybe.withDefault 0
        in
        Html.div [ HA.class "rg-history" ]
            (Html.div [ HA.class "rg-history-title" ]
                [ Html.text ("Recent delves — best score " ++ String.fromInt best) ]
                :: List.map runRow runs
            )


{-| Lifetime achievements, each unlocked by a finished run meeting its condition. -}
badgeDefs : List { id : String, label : String, desc : String, earned : Run -> Bool }
badgeDefs =
    [ { id = "first-blood", label = "First Blood", desc = "Slay a monster", earned = \r -> r.kills >= 1 }
    , { id = "delver", label = "Delver", desc = "Reach depth 3", earned = \r -> r.depth >= 3 }
    , { id = "spelunker", label = "Spelunker", desc = "Reach depth 5", earned = \r -> r.depth >= 5 }
    , { id = "deep-diver", label = "Deep Diver", desc = "Reach depth 8", earned = \r -> r.depth >= 8 }
    , { id = "slayer", label = "Slayer", desc = "25 kills in a run", earned = \r -> r.kills >= 25 }
    , { id = "survivor", label = "Survivor", desc = "Survive 400 turns", earned = \r -> r.turns >= 400 }
    , { id = "abyssal", label = "Abyssal", desc = "Reach depth 10", earned = \r -> r.depth >= 10 }
    , { id = "centurion", label = "Centurion", desc = "100 kills in a run", earned = \r -> r.kills >= 100 }
    , { id = "marathoner", label = "Marathoner", desc = "Survive 800 turns", earned = \r -> r.turns >= 800 }
    , { id = "victor", label = "Victor", desc = "Claim the Amulet", earned = \r -> r.won }
    , { id = "swift-victor", label = "Swift Victor", desc = "Win in under 600 turns", earned = \r -> r.won && r.turns < 600 }
    ]


{-| Badge ids a single finished run unlocks. -}
badgesFromRun : Run -> Set String
badgesFromRun run =
    badgeDefs |> List.filter (\b -> b.earned run) |> List.map .id |> Set.fromList


badgesView : Set String -> Html Msg
badgesView earned =
    Html.div [ HA.class "rg-history" ]
        [ Html.div [ HA.class "rg-history-title" ]
            [ Html.text ("Achievements (" ++ String.fromInt (List.length (List.filter (\b -> Set.member b.id earned) badgeDefs)) ++ "/" ++ String.fromInt (List.length badgeDefs) ++ ")") ]
        , Html.div [ HA.class "rg-badges" ]
            (List.map
                (\b ->
                    Html.div
                        [ HA.class
                            (if Set.member b.id earned then
                                "rg-badge"

                             else
                                "rg-badge is-locked"
                            )
                        ]
                        [ Html.div [ HA.class "rg-badge-name" ] [ Html.text b.label ]
                        , Html.div [ HA.class "rg-badge-desc" ] [ Html.text b.desc ]
                        ]
                )
                badgeDefs
            )
        ]


runRow : Run -> Html Msg
runRow run =
    Html.div [ HA.class "rg-run" ]
        [ Html.span
            [ HA.class "rg-run-result"
            , HA.style "color"
                (if run.won then
                    "#5dd47a"

                 else
                    "#e0564b"
                )
            ]
            [ Html.text
                (if run.won then
                    "VICTORY"

                 else
                    "DIED"
                )
            ]
        , Html.span [ HA.class "rg-run-class" ] [ Html.text run.className ]
        , Html.span [] [ Html.text ("depth " ++ String.fromInt run.depth ++ " · " ++ String.fromInt run.kills ++ " slain · " ++ String.fromInt run.turns ++ "t · " ++ String.fromInt (scoreFor run) ++ " pts") ]
        ]



-- INPUT ------------------------------------------------------------------------------------------


{-| Monster ids the hero can currently see — folded into the run's bestiary each step. -}
seenMonsters : Game -> Set String
seenMonsters game =
    game.enemies
        |> List.filter (\e -> Set.member ( e.pos.x, e.pos.y ) game.visible)
        |> List.map (\e -> e.def.id)
        |> Set.fromList




keyToGameMsg : String -> Game.Msg
keyToGameMsg key =
    case String.toLower key of
        "arrowup" ->
            Game.Move Grid.dirN

        "arrowdown" ->
            Game.Move Grid.dirS

        "arrowleft" ->
            Game.Move Grid.dirW

        "arrowright" ->
            Game.Move Grid.dirE

        "w" ->
            Game.Move Grid.dirN

        "s" ->
            Game.Move Grid.dirS

        "a" ->
            Game.Move Grid.dirW

        "d" ->
            Game.Move Grid.dirE

        "k" ->
            Game.Move Grid.dirN

        "j" ->
            Game.Move Grid.dirS

        "h" ->
            Game.Move Grid.dirW

        "l" ->
            Game.Move Grid.dirE

        "y" ->
            Game.Move Grid.dirNW

        "u" ->
            Game.Move Grid.dirNE

        "b" ->
            Game.Move Grid.dirSW

        "n" ->
            Game.Move Grid.dirSE

        "." ->
            Game.Wait

        "z" ->
            Game.Search

        "f" ->
            Game.Fire

        "c" ->
            Game.Brew

        "q" ->
            Game.Ability

        "x" ->
            Game.Examine

        "o" ->
            Game.AutoExplore

        ">" ->
            Game.Descend

        "r" ->
            Game.Restart

        _ ->
            case String.toInt key of
                Just digit ->
                    if digit >= 1 && digit <= 9 then
                        Game.Use (digit - 1)

                    else
                        Game.NoOp

                Nothing ->
                    Game.NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))
        , -- The 3D renderer animates continuously; only pay for an animation-frame clock when it's on.
          if model.screen == Playing && model.rendererName == "3D" then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
