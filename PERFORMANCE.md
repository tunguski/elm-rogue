# elm-rogue — performance notes & optimization analysis

How to keep rendering seamless and the game fast even on slow machines. This documents what the code
already does, what was changed in the M41 pass, and the highest-value work still on the table.

## The shape of the problem

elm-rogue is **turn-based**, so it only re-renders in response to a keypress — there is no
per-frame animation loop and no idle CPU use. The cost that matters is therefore **per-keypress
latency**, which is:

```
keypress → Game.update (pure)  →  Game.toScene (pure)  →  Renderer.view → Html  →  vDOM diff → paint
```

On a slow machine the dominant terms are usually the **vDOM diff/paint of the SVG node count** and
any **O(map)** scans done every keypress. The engine step itself is cheap (small records, a few
`Set`/`Dict` ops). So the optimization strategy is: *render fewer nodes* and *never do work
proportional to the whole map when it can be proportional to what's on screen or what's changed.*

## What already helps

- **Turn-based, event-driven rendering.** No `requestAnimationFrame`/`Time.every` redraw loop, so the
  GPU/CPU is idle between moves. (The one place a clock could creep in — animations — is deliberately
  avoided; combat "floating numbers" are one-frame, state-driven, not tweened.)
- **Renderer-agnostic `Scene`.** The engine emits a flat data `Scene` once per move; the renderer is
  pure `Scene → Html`. This keeps the hot path allocation-light and makes the renderer swappable
  (the ASCII renderer is markedly cheaper than SVG on very weak machines — it's one `<span>` per cell
  with no strokes/filters).
- **Pure, seed-threaded engine.** No effects in the step; `update` is a cheap fold over small lists.

## Changes made in the M41 pass

1. **Viewport culling (M35).** The SVG renderer draws only a camera-centred **31×21 window** of cells
   instead of the whole 40×26 floor — and, crucially, this is **constant regardless of map size**, so
   bigger future floors cost nothing extra. The ASCII renderer got the same windowing (a 41×23
   window) in M41; previously it emitted a `<span>` for every map cell (~1040), now ~940 worst case
   and bounded.
2. **Skip unseen cells.** `cellSvg` now returns `Nothing` for never-seen cells and lets the SVG's own
   background show through. Early in a floor (most of the viewport unexplored) this cuts the node
   count dramatically; node count now scales with *explored area in view*, not the viewport rectangle.
3. **Minimap iterates the explored `Set`, not the whole map.** It previously scanned all 1040 cells
   every keypress; it now folds over `Set.toList explored`, so its cost scales with how much you've
   actually seen. The hero marker is drawn as one extra node on top.

Net effect: on an unexplored early floor the SVG map went from ~1040 always-drawn rects to a few
dozen; fully explored, it's bounded by the ~650-cell viewport instead of the whole floor; and the
minimap no longer does a full-map scan per move.

## Further opportunities (ranked by value)

1. **`Html.Lazy` on the static panels.** The HUD stats, message log, minimap and inventory don't
   change on a plain move. Wrapping them in `Html.Lazy.lazy` would let the vDOM skip diffing those
   subtrees. The catch in the current data flow: `Game.toScene` rebuilds `Set`s/lists each move, so
   the lazy args aren't referentially stable and the memo would always miss. The fix is to thread a
   few **stable references** (e.g. keep `explored`/`hud` as the same value object when unchanged, or
   pass a cheap version counter that only bumps when the panel's inputs change) and key `lazy` on
   that. This is the single biggest remaining win for slow machines because it removes the HUD/minimap
   from the per-keypress diff entirely.
2. **One background rect + sparse foreground.** Instead of per-cell `stroke`/`fill` rects, paint a
   single dark background rect for the viewport and only emit rects for lit/remembered floor and
   walls. Fewer nodes and far less stroke rasterization (strokes are comparatively expensive to
   paint).
3. **Glyphs as one `<text>` per row.** Monsters/items in a row could be merged into a single
   positioned `<text>` with spacing, cutting `<text>` node count. (Marginal unless many entities.)
4. **Shadowcasting FOV.** `Rogue.Fov.compute` is an O(cells·radius) raycast that allocates a Bresenham
   list per target cell. Recursive shadowcasting is O(cells) with no per-cell line allocation. At the
   current radius (7) and map size this isn't a bottleneck, but it's the right call before large maps
   or large light radii.
5. **Incremental `explored`.** `refreshFov` does `Set.union explored vis` each move (allocates a new
   set). Folding only the *newly* visible keys into `explored` avoids re-hashing the whole set; minor
   today, but it also gives you the stable reference #1 wants when nothing new was seen.
6. **`requestAnimationFrame` batching for key-repeat.** Holding a movement key fires many `onKeyDown`s;
   coalescing bursts (process at most one per frame) keeps a slow machine from queuing a backlog of
   renders. A `Browser.Events.onAnimationFrame` gate on input would smooth fast travel.
7. **Crisp, cheap paint hints.** `image-rendering: pixelated` on the container and avoiding SVG
   `filter`/`drop-shadow` (already avoided) keep rasterization cheap; a CSS `contain: strict` on the
   map box limits layout/paint scope.

## Rule of thumb for contributors

Anything in the per-keypress path (`update`/`toScene`/`view`) must be **O(viewport or O(Δ)**, never
**O(map)**. If you add a HUD widget, make it `lazy`-able. If you add terrain detail, render it inside
the culled viewport loop, not a fresh full-map scan. The headless suite's connectivity fuzz test
([test/RogueTest.elm](test/RogueTest.elm)) guards correctness while you optimize.
