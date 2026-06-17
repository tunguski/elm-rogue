#!/usr/bin/env bash
#
# smoke.sh — headless integration smoke test for elm-rogue.
#
# Builds the app, starts a game, presses ONLY movement keys, and asserts the turn counter advanced.
# This guards the keypress -> keyToGameMsg -> Game.update path through the *built JS* — the layer the
# pure `elm test` suite cannot reach. It exists because of a real regression: a cross-module name
# collision once made `Game.Move` miscompile, so every movement key silently became a no-op while the
# unit tests stayed green. Pressing no Wait/Search key here means only genuine movement (or a bump
# attack) can advance the turn, so a broken Move path makes this test fail.
#
# Requires headless Chrome. Usage:
#   ELM=../../elm.sh ./smoke.sh
# Override Chrome with CHROME=/path/to/chrome.
set -euo pipefail
cd "$(dirname "$0")"

ELM="${ELM:-elm}"
CHROME="${CHROME:-/c/Program Files/Google/Chrome/Application/chrome.exe}"

ELM="$ELM" ./build.sh >/dev/null
echo "built; running smoke test..."

OUT="build/smoke.html"
cp build/elm-rogue.html "$OUT"
cat >> "$OUT" <<'HTML'
<script>
// Fire keydown on both window and document so it reaches Elm's Browser.Events.onKeyDown listener.
function press(k){
  window.dispatchEvent(new KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));
  document.dispatchEvent(new KeyboardEvent('keydown', { key: k, bubbles: true, cancelable: true }));
}
setTimeout(function () {
  var card = document.querySelector('.rg-card');   // start a game with the first class
  if (card) { card.click(); }
  setTimeout(function () {
    // Movement keys only — no Wait/Search — so the turn advances only if a real move/attack happened.
    ['d','s','a','w','h','j','k','l','y','u','b','n','ArrowRight','ArrowDown','ArrowLeft','ArrowUp'].forEach(press);
  }, 250);
}, 400);
</script>
HTML

# Chrome needs a Windows-style path for file:// URLs from Git Bash.
WIN="$(pwd -W 2>/dev/null || pwd | sed 's#^/\([a-zA-Z]\)/#\1:/#')/build/smoke.html"
ERRLOG="$(mktemp)"
DOM="$("$CHROME" --headless=new --disable-gpu --no-sandbox --virtual-time-budget=4000 \
        --enable-logging=stderr --v=0 "file:///$WIN" --dump-dom 2>"$ERRLOG" || true)"

TURN="$(echo "$DOM" | grep -oE 'Turn</span><span>[0-9]+' | grep -oE '[0-9]+$' | head -1)"
UNBOUND="$(grep -oE 'Unbound: [A-Za-z.]+' "$ERRLOG" | head -1 || true)"
rm -f "$ERRLOG" "$OUT"

if [ -n "$UNBOUND" ]; then
  echo "SMOKE FAIL: runtime error in the built app — $UNBOUND"
  exit 1
fi
if [ -z "$TURN" ]; then
  echo "SMOKE FAIL: could not read the turn counter (did the game start and render?)"
  exit 1
fi
if [ "$TURN" -lt 1 ]; then
  echo "SMOKE FAIL: movement keys did not advance the turn (turn=$TURN) — keypress->Move path is broken."
  exit 1
fi
echo "SMOKE PASS: movement advanced the turn to $TURN."
