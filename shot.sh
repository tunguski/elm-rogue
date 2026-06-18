#!/usr/bin/env bash
# Screenshot harness: start a game (optionally switch renderer), walk a few steps to reveal a
# torch-lit room, then capture a headless screenshot. Usage:  ./shot.sh [SVG|ASCII|3D] [out.png]
set -euo pipefail
cd "$(dirname "$0")"
CHROME="${CHROME:-/c/Program Files/Google/Chrome/Application/chrome.exe}"
REND="${1:-SVG}"
OUT="${2:-build/shot-$REND.png}"

cp build/elm-rogue.html build/shot.html
cat >> build/shot.html <<EOF
<script>
function press(k){ window.dispatchEvent(new KeyboardEvent('keydown',{key:k,bubbles:true})); document.dispatchEvent(new KeyboardEvent('keydown',{key:k,bubbles:true})); }
setTimeout(function(){
  var card=document.querySelector('.rg-card'); if(card) card.click();
  setTimeout(function(){
    var want="$REND";
    [].slice.call(document.querySelectorAll('button')).filter(function(x){return x.textContent.trim()===want;}).forEach(function(b){b.click();});
    setTimeout(function(){ ['d','d','s','s','a','s','d'].forEach(press); }, 150);
  }, 250);
}, 350);
</script>
EOF

WIN="$(pwd -W)/build/shot.html"
"$CHROME" --headless=new --disable-gpu --no-sandbox --enable-unsafe-swiftshader \
  --screenshot="$(pwd -W)/$OUT" --window-size=1280,860 --virtual-time-budget=5000 "file:///$WIN" >/dev/null 2>&1 || true
rm -f build/shot.html
echo "wrote $OUT"
