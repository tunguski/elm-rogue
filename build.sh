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

# The compiler owns the output's <head> (just charset + title), so we post-process it: inject a
# viewport meta (else phones render at desktop width) and inline src/app.css as a <style> (the app's
# styling lives in that file as classes; the page stays a single self-contained HTML).
HTML="$P/$OUT/elm-rogue.html"
CSSFILE="$P/src/app.css" perl -0pi -e '
  if (index($_, q{name="viewport"}) < 0) {
    s#<meta charset="utf-8">#<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">#;
  }
  if (index($_, q{id="rg-app-css"}) < 0) {
    open(my $f, "<", $ENV{CSSFILE}) or die "no app.css: $!";
    local $/; my $css = <$f>; close($f);
    s#</head>#"<style id=\"rg-app-css\">".$css."</style></head>"#e;
  }
' "$HTML"
echo "Done -> $OUT/elm-rogue.html"
