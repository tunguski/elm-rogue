module Rogue.Game.Appearance exposing (..)

{-| Pure item appearance & naming: the per-run disguises for potions/scrolls/rings/wands, the
masked display name/colour shown until an item is identified, and the item-classification
predicates. Shared by the engine and the scene projection, so it sits below both. -}

import Dict exposing (Dict)
import Rogue.Content as Content exposing (ItemDef, Ruleset)
import Rogue.Game.Types exposing (..)
import Rogue.Rng as Rng exposing (Seed)
import Set exposing (Set)


{-| The pool of random looks an unidentified potion can wear this run. -}
palette : List Appearance
palette =
    [ { adjective = "murky", color = "#7f8b6a" }
    , { adjective = "azure", color = "#4f8bff" }
    , { adjective = "crimson", color = "#e0564b" }
    , { adjective = "fizzy", color = "#5dd47a" }
    , { adjective = "golden", color = "#d8b24c" }
    , { adjective = "violet", color = "#9b6ad8" }
    , { adjective = "smoky", color = "#9aa7ba" }
    , { adjective = "amber", color = "#e0824b" }
    , { adjective = "inky", color = "#6a6f86" }
    , { adjective = "milky", color = "#d6d2c2" }
    , { adjective = "bubbling", color = "#6ad8c0" }
    , { adjective = "charcoal", color = "#4a4f5e" }
    , { adjective = "cloudy", color = "#aeb6c2" }
    , { adjective = "ivory", color = "#e6e0cf" }
    ]


{-| The pool of runic labels an unidentified scroll can wear this run. -}
scrollPalette : List Appearance
scrollPalette =
    [ { adjective = "GORO", color = "#c9b88a" }
    , { adjective = "KAUNAN", color = "#caa472" }
    , { adjective = "OYEE", color = "#b8c46a" }
    , { adjective = "ZID", color = "#9ab0d6" }
    , { adjective = "TIWAZ", color = "#d4a06a" }
    , { adjective = "ELAR", color = "#a7c46a" }
    , { adjective = "VARK", color = "#c79ad6" }
    , { adjective = "WERG", color = "#d6c27a" }
    , { adjective = "NYX", color = "#9aa7ba" }
    , { adjective = "RETH", color = "#caa0a0" }
    , { adjective = "MOTH", color = "#b8c0a0" }
    , { adjective = "QORN", color = "#c0a0c8" }
    , { adjective = "FENG", color = "#a0c0c0" }
    , { adjective = "ULAR", color = "#c8b890" }
    , { adjective = "DROV", color = "#b0a8c8" }
    , { adjective = "SKAL", color = "#c8a890" }
    , { adjective = "BRIX", color = "#a0c8b0" }
    , { adjective = "VOTH", color = "#c8c090" }
    ]


{-| Is this item a potion (and so subject to identification)? -}
isPotion : ItemDef -> Bool
isPotion def =
    case def.kind of
        Content.Consumable _ ->
            String.startsWith "potion" def.id

        _ ->
            False


{-| Is this item a scroll? -}
isScroll : ItemDef -> Bool
isScroll def =
    case def.kind of
        Content.Consumable _ ->
            String.startsWith "scroll" def.id

        _ ->
            False


isRing : ItemDef -> Bool
isRing def =
    case def.kind of
        Content.Equipment Content.RingSlot _ ->
            True

        _ ->
            False


isWand : ItemDef -> Bool
isWand def =
    case def.kind of
        Content.Wand _ ->
            True

        _ ->
            False


{-| The gem/wood labels unidentified rings and wands wear this run. -}
ringPalette : List Appearance
ringPalette =
    [ { adjective = "diamond", color = "#9be0ff" }
    , { adjective = "ruby", color = "#e0564b" }
    , { adjective = "emerald", color = "#5dd47a" }
    , { adjective = "topaz", color = "#d8b24c" }
    , { adjective = "agate", color = "#c79ad6" }
    , { adjective = "onyx", color = "#9aa7ba" }
    , { adjective = "sapphire", color = "#4f8bff" }
    , { adjective = "garnet", color = "#caa0a0" }
    ]


wandPalette : List Appearance
wandPalette =
    [ { adjective = "yew", color = "#caa472" }
    , { adjective = "ebony", color = "#6a6f86" }
    , { adjective = "birch", color = "#d6d2c2" }
    , { adjective = "holly", color = "#5dd47a" }
    , { adjective = "willow", color = "#a7c46a" }
    , { adjective = "rowan", color = "#e0824b" }
    , { adjective = "teak", color = "#b8895a" }
    , { adjective = "elm", color = "#9ab0d6" }
    , { adjective = "alder", color = "#c0a060" }
    , { adjective = "cedar", color = "#d08a5a" }
    ]


{-| Items subject to per-run identification (potions, scrolls, rings and wands). -}
unidentifiable : ItemDef -> Bool
unidentifiable def =
    isPotion def || isScroll def || isRing def || isWand def


{-| Assign each randomized item id a distinct appearance for the run. -}
assignLooks : Ruleset -> Seed -> ( Dict String Appearance, Seed )
assignLooks ruleset seed =
    let
        idsOf pred =
            ruleset.items |> List.filter pred |> List.map .id

        ( potionLooks, seed1 ) =
            Rng.shuffle palette seed

        ( scrollLooks, seed2 ) =
            Rng.shuffle scrollPalette seed1

        ( ringLooks, seed3 ) =
            Rng.shuffle ringPalette seed2

        ( wandLooks, seed4 ) =
            Rng.shuffle wandPalette seed3
    in
    ( Dict.fromList
        (List.map2 Tuple.pair (idsOf isPotion) potionLooks
            ++ List.map2 Tuple.pair (idsOf isScroll) scrollLooks
            ++ List.map2 Tuple.pair (idsOf isRing) ringLooks
            ++ List.map2 Tuple.pair (idsOf isWand) wandLooks
        )
    , seed4
    )


lookAdjective : Idents -> ItemDef -> String
lookAdjective idents def =
    Dict.get def.id idents.looks |> Maybe.map .adjective |> Maybe.withDefault ""


{-| The name to show for an item: its true name once identified, else its per-run disguised appearance
("<adjective> potion", "scroll labeled <RUNE>", "<gem> ring", "<wood> wand"). -}
displayName : Idents -> ItemDef -> String
displayName idents def =
    let
        known =
            Set.member def.id idents.known
    in
    case def.kind of
        Content.Wand spec ->
            (if known then
                def.name

             else
                lookAdjective idents def ++ " wand"
            )
                ++ " ("
                ++ String.fromInt spec.charges
                ++ ")"

        Content.Artifact spec ->
            def.name
                ++ (if spec.charge >= spec.maxCharge then
                        " ✦"

                    else
                        " (" ++ String.fromInt spec.charge ++ "/" ++ String.fromInt spec.maxCharge ++ ")"
                   )

        Content.Equipment Content.RingSlot bonus ->
            if known then
                def.name ++ plusSuffix bonus

            else
                lookAdjective idents def ++ " ring"

        Content.Equipment _ bonus ->
            def.name ++ plusSuffix bonus

        _ ->
            if not (unidentifiable def) || known then
                def.name

            else if isScroll def then
                "scroll labeled " ++ lookAdjective idents def

            else
                lookAdjective idents def ++ " potion"


plusSuffix : Content.EquipBonus -> String
plusSuffix bonus =
    if bonus.plus > 0 then
        " +" ++ String.fromInt bonus.plus

    else
        ""


{-| The colour to draw an item with: true colour once identified, else its appearance colour. -}
displayColor : Idents -> ItemDef -> String
displayColor idents def =
    if not (unidentifiable def) || Set.member def.id idents.known then
        def.color

    else
        case Dict.get def.id idents.looks of
            Just look ->
                look.color

            Nothing ->
                def.color