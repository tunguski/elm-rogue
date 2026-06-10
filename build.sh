#!/usr/bin/env bash
#
# build.sh — compile elm-rogue to a standalone HTML file with the elm-lang CLI.
#
# The elm.sh wrapper chdirs to the elm-lang repo root before running, so every path passed to
# `make` must be absolute (computed here after we cd into the script's own directory). Like the
# other elm-lang example apps we compile with --no-check.
#
#   ELM=../../elm.sh ./build.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
OUT="build"
P="$(pwd)"

mkdir -p "$OUT"
echo "Compiling elm-rogue with: $ELM"
$ELM make "$P/src/Main.elm" --project="$P/elm.json" -o "$P/$OUT/elm-rogue.html" --no-check
echo "Done -> $OUT/elm-rogue.html"
