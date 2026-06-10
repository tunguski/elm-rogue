module Rogue.Rng exposing
    ( Seed
    , seed
    , step
    , int
    , range
    , chance
    , oneIn
    , pick
    , pickWeighted
    , shuffle
    , split
    )

{-| A tiny deterministic pseudo-random number generator (a 32-bit linear congruential generator).

The project does not depend on `elm/random`, and the whole game must be reproducible (so a seed
fully determines a dungeon, the enemy placement and every AI tie-break, and the engine can be tested
headlessly). A `Seed` is therefore just an `Int`: every draw returns the value *and* the next seed.
Threading the seed by hand keeps the code interpreter-friendly and the data trivially inspectable.

Adapted from the elm-rts example's `RTS.Rng`, extended with `oneIn`, `pickWeighted` and `split`.
-}


{-| The generator state — opaque in spirit, but just an `Int` so it can live in the model and tests
can pin it to a literal. -}
type alias Seed =
    Int


{-| Normalise an arbitrary integer into a usable seed (kept in the LCG's 32-bit range, never zero). -}
seed : Int -> Seed
seed n =
    let
        s =
            modBy 4294967296 (abs n)
    in
    if s == 0 then
        2463534242

    else
        s


{-| Advance the state once (Numerical-Recipes LCG constants, modulo 2^32). -}
step : Seed -> Seed
step s =
    modBy 4294967296 (1664525 * s + 1013904223)


{-| A non-negative integer in `[0, n)` and the next seed. The high bits of the state are used (the
low bits of an LCG cycle short), so successive small draws are well spread. -}
int : Int -> Seed -> ( Int, Seed )
int n s =
    if n <= 1 then
        ( 0, step s )

    else
        let
            s2 =
                step s
        in
        ( modBy n (s2 // 256), s2 )


{-| An integer in the inclusive range `[lo, hi]` and the next seed. -}
range : Int -> Int -> Seed -> ( Int, Seed )
range lo hi s =
    if hi <= lo then
        ( lo, step s )

    else
        let
            ( d, s2 ) =
                int (hi - lo + 1) s
        in
        ( lo + d, s2 )


{-| `True` with probability `p` (a percentage in `[0, 100]`), and the next seed. -}
chance : Int -> Seed -> ( Bool, Seed )
chance p s =
    let
        ( r, s2 ) =
            int 100 s
    in
    ( r < p, s2 )


{-| `True` with probability `1/n` (e.g. `oneIn 8` is a one-in-eight chance), and the next seed. -}
oneIn : Int -> Seed -> ( Bool, Seed )
oneIn n s =
    let
        ( r, s2 ) =
            int (max 1 n) s
    in
    ( r == 0, s2 )


{-| Pick a random element of a list (the default is returned for an empty list), and the next seed. -}
pick : a -> List a -> Seed -> ( a, Seed )
pick default xs s =
    case xs of
        [] ->
            ( default, step s )

        _ ->
            let
                ( i, s2 ) =
                    int (List.length xs) s
            in
            ( nth i default xs, s2 )


{-| Pick from a list of `( weight, value )` pairs proportionally to weight. Non-positive weights are
ignored; the default is returned if nothing has positive weight. Also returns the next seed. -}
pickWeighted : a -> List ( Int, a ) -> Seed -> ( a, Seed )
pickWeighted default pairs s =
    let
        total =
            List.foldl (\( w, _ ) acc -> acc + max 0 w) 0 pairs
    in
    if total <= 0 then
        ( default, step s )

    else
        let
            ( r, s2 ) =
                int total s
        in
        ( weightedAt r default pairs, s2 )


weightedAt : Int -> a -> List ( Int, a ) -> a
weightedAt r default pairs =
    case pairs of
        [] ->
            default

        ( w, v ) :: rest ->
            let
                ww =
                    max 0 w
            in
            if r < ww then
                v

            else
                weightedAt (r - ww) default rest


{-| A Fisher–Yates shuffle of the list, plus the next seed. -}
shuffle : List a -> Seed -> ( List a, Seed )
shuffle xs s =
    shuffleHelp xs [] s


shuffleHelp : List a -> List a -> Seed -> ( List a, Seed )
shuffleHelp remaining acc s =
    case remaining of
        [] ->
            ( acc, s )

        head :: _ ->
            let
                ( i, s2 ) =
                    int (List.length remaining) s

                picked =
                    nth i head remaining
            in
            shuffleHelp (removeAt i remaining) (picked :: acc) s2


{-| Derive a second, independent seed from this one (for sub-generators), and advance the original. -}
split : Seed -> ( Seed, Seed )
split s =
    let
        a =
            step s

        b =
            step (a + 0x9E3779B9)
    in
    ( seed b, a )


nth : Int -> a -> List a -> a
nth i default xs =
    case List.head (List.drop i xs) of
        Just x ->
            x

        Nothing ->
            default


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs
