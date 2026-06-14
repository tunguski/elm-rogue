module SpriteGallery exposing (main)

{-| A throwaway dev harness: renders every sprite in `Rogue.Sprite` at a large scale on a dark grid,
so the pixel art can be eyeballed and iterated without launching the whole game. Not part of the build
(`build.sh` compiles `Main.elm`); compile it on demand with the elm.jar and screenshot it. -}

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Rogue.Sprite as Sprite
import Svg exposing (svg, g, rect, text_)
import Svg.Attributes as SA


main : Program () () ()
main =
    Browser.sandbox { init = (), update = \_ m -> m, view = \_ -> view }


keys : List String
keys =
    [ "hero"
    , "hero-warrior"
    , "hero-mage"
    , "hero-rogue"
    , "hero-huntress"
    , "hero-duelist"
    , "rat"
    , "snake"
    , "gnoll-scout"
    , "gnoll-brute"
    , "gnoll-shaman"
    , "crab"
    , "skeleton"
    , "swarm"
    , "slime"
    , "thief"
    , "prison-guard"
    , "cave-bat"
    , "piranha"
    , "stone-golem"
    , "necromancer"
    , "dwarf-monk"
    , "demon"
    , "succubus"
    , "ghost-ally"
    , "!"
    , "?"
    , "/"
    , "["
    , "="
    , "-"
    , "%"
    , "$"
    ]


cell : Int
cell =
    72


view : Html ()
view =
    let
        cols =
            7

        tile i key =
            let
                row =
                    i // cols

                col =
                    modBy cols i

                spr =
                    Sprite.resolve key ""

                art =
                    case spr of
                        Just s ->
                            Sprite.toSvg cell { x = 0, y = 0 } "" 0 s

                        Nothing ->
                            []
            in
            g [ SA.transform ("translate(" ++ String.fromInt (col * (cell + 16) + 8) ++ "," ++ String.fromInt (row * (cell + 28) + 8) ++ ")") ]
                (rect [ SA.x "0", SA.y "0", SA.width (String.fromInt cell), SA.height (String.fromInt cell), SA.fill "#0e1320", SA.stroke "#26304a" ] []
                    :: art
                    ++ [ text_ [ SA.x (String.fromInt (cell // 2)), SA.y (String.fromInt (cell + 16)), SA.fill "#8a93a6", SA.fontSize "12", SA.textAnchor "middle", SA.fontFamily "monospace" ] [ Svg.text key ] ]
                )

        rows =
            (List.length keys + cols - 1) // cols

        w =
            cols * (cell + 16) + 8

        h =
            rows * (cell + 28) + 8
    in
    Html.div [ HA.style "background" "#05070b", HA.style "padding" "12px" ]
        [ svg
            [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
            , SA.width (String.fromInt w)
            , SA.height (String.fromInt h)
            ]
            (List.indexedMap tile keys)
        ]
