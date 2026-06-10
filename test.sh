#!/usr/bin/env bash
#
# test.sh — run the elm-rouge headless test suite (pure engine checks: RNG, grid, level, dungeon
# connectivity, newGame). The runner needs the test file plus every source module it touches.
#
#   ELM=../../elm.sh ./test.sh
#
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"

$ELM test test/RogueTest.elm \
  src/Rogue/Rng.elm src/Rogue/Grid.elm src/Rogue/Tile.elm src/Rogue/Level.elm \
  src/Rogue/Fov.elm src/Rogue/Path.elm src/Rogue/Dungeon.elm src/Rogue/Content.elm \
  src/Rogue/Render.elm src/Rogue/Game.elm src/Mod/Default.elm
