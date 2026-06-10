#!/usr/bin/env bash
#
# test.sh — run the elm-rogue headless test suite (pure engine checks: RNG, grid, level, dungeon
# connectivity, newGame). The runner needs the test file plus every source module it touches.
#
#   ELM=../../elm.sh ./test.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
P="$(pwd)"

# Absolute paths: the elm.sh wrapper chdirs to the elm-lang repo root before running.
$ELM test "$P/test/RogueTest.elm" \
  "$P/src/Rogue/Rng.elm" "$P/src/Rogue/Grid.elm" "$P/src/Rogue/Tile.elm" "$P/src/Rogue/Level.elm" \
  "$P/src/Rogue/Fov.elm" "$P/src/Rogue/Path.elm" "$P/src/Rogue/Dungeon.elm" "$P/src/Rogue/Content.elm" \
  "$P/src/Rogue/Render.elm" "$P/src/Rogue/Game.elm" "$P/src/Mod/Default.elm"
