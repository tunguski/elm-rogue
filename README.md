# elm-rogue — a moddable roguelike in Elm

A turn-based, grid dungeon crawler in the spirit of
[Shattered Pixel Dungeon](https://github.com/00-Evan/shattered-pixel-dungeon), written for the
[elm-lang](https://github.com/tunguski/elm-lang) implementation of Elm and built so that **the
content and the renderer are both mods**: the bestiary, the items and the hero are plain data
(`Ruleset`), and the graphics are a swappable record of functions (`Renderer`). The simulation never
names SVG, ASCII, a specific monster or a specific potion — it reads everything from injected values.

> Built iteratively in milestones. This is the first set of ten (a playable, moddable core); more
> follow until it reaches Pixel-Dungeon depth.

## Play

Build a standalone HTML file with the elm-lang CLI and open it:

```sh
ELM=../../elm.sh ./build.sh          # or: elm make src/Main.elm --project=elm.json -o build/elm-rogue.html --no-check
# then open build/elm-rogue.html
```

**Controls** — arrows / `WASD` / `HJKL` to move, `Y U B N` for diagonals, `.` to wait, `>` to
descend stairs, `1`–`9` to use an inventory item, `R` to restart. Walk into a monster to attack it.
Reach depth 8 to win; die and you start a fresh dungeon. The toolbar switches the **Mod** and the
**Renderer** live.

## How it plays

- **Pick a class** — Warrior, Mage or Rogue (or a mod's own), each with its own stats, FOV and opening
  gear. Finished runs are saved to `localStorage` and listed on the class screen.
- **Seeded dungeons** — rooms carved from rock, linked by L-corridors, walls auto-finished, up/down
  stairs placed. The look changes by region (Sewers → Prison → Caves → Halls). The same seed always
  yields the same floor (so the engine is reproducible/testable).
- **Fog of war** — a raycast field of view lights the cells you can see; everything you have seen is
  remembered (dimmed) and the rest is black.
- **Turn-based monsters** — each wakes on line of sight, stays alert, and **BFS-paths** around corners
  to reach you; melee bumps, archers shoot, the badly wounded flee. HP/damage/defense come from data.
- **Items & gear** — auto-pick-up into an inventory; **equip** weapons and armour (derived
  attack/defense), **drink** potions (heal, strength, shielding, regeneration), **read** scrolls
  (teleport, magic mapping, identify), and **zap** a charged wand at the nearest visible monster.
- **Roguelike systems** — hidden **traps** (dart/poison/teleport) you can **search** for; timed
  **status effects** (poison/burn/regen) that tick each turn; **XP & leveling** (HP/damage growth on
  level-up); and **unidentified potions** with per-run random appearances you learn by drinking.

## Architecture — the two mod seams

Elm has no runtime code loading, so "moddability" here means **data in, behaviour out**, with two
clean boundaries the engine is parameterised over.

### 1. Content is a `Ruleset` (data)

[`Rogue.Content`](src/Rogue/Content.elm) defines `EnemyDef`, `ItemDef`, `ItemEffect` and the
`Ruleset` that bundles them with the hero's stats. A mod is just a `Ruleset` value:

- [`Mod.Default`](src/Mod/Default.elm) — the stock bestiary and items, authored from scratch.
- [`Mod.Hard`](src/Mod/Hard.elm) — *derived* from `Mod.Default` by `map`-ping over its roster
  (tougher monsters, scarcer healing). Mods can transform other mods.

"Custom strengths of enemies, weapons and items" is therefore literally just different numbers in a
`Ruleset`; the engine ([`Rogue.Game`](src/Rogue/Game.elm)) takes the ruleset as a parameter and
spawns, fights and resolves items entirely from it. A new *kind* of effect is the one thing that
needs an engine touch: add a constructor to `ItemEffect` and its case in `Rogue.Game.applyEffect`.

### 2. Rendering is a `Renderer` (record of functions)

[`Rogue.Render`](src/Rogue/Render.elm) defines a renderer-agnostic **`Scene`** (the terrain, the
visible/explored sets, the drawable `Glyph`s, and a `Hud`) and a **`Renderer msg`** record whose
`view : Scene -> Html msg` turns a scene into pixels. The engine only ever produces a `Scene`:

- [`Rogue.Render.Svg`](src/Rogue/Render/Svg.elm) — crisp SVG tiles + an HTML HUD (the default).
- [`Rogue.Render.Ascii`](src/Rogue/Render/Ascii.elm) — a classic coloured text-mode view.

Both consume the identical `Scene`, so swapping them changes nothing in the simulation — the
"alternative game rendering engine" extension point. `Main` lists the installed renderers and lets
you pick one at runtime.

### Module map

| Module | Role |
|---|---|
| [`Rogue.Rng`](src/Rogue/Rng.elm) | Deterministic LCG RNG (no `elm/random`); seed threaded by hand. |
| [`Rogue.Grid`](src/Rogue/Grid.elm) | Positions, directions, distances, Bresenham lines. |
| [`Rogue.Tile`](src/Rogue/Tile.elm) | The closed terrain vocabulary + passability/sight. |
| [`Rogue.Level`](src/Rogue/Level.elm) | Sparse tile grid with bounds-safe access. |
| [`Rogue.Dungeon`](src/Rogue/Dungeon.elm) | Seeded room+corridor generator. |
| [`Rogue.Fov`](src/Rogue/Fov.elm) | Raycast field of view. |
| [`Rogue.Content`](src/Rogue/Content.elm) | **Moddable content types** (`Ruleset`, `EnemyDef`, `ItemDef`). |
| [`Rogue.Game`](src/Rogue/Game.elm) | The pure engine: state, movement, combat, AI, items, `toScene`. |
| [`Rogue.Render`](src/Rogue/Render.elm) | **The `Scene`/`Renderer` seam.** |
| [`Rogue.Render.Svg`](src/Rogue/Render/Svg.elm) / [`.Ascii`](src/Rogue/Render/Ascii.elm) | Two rendering engines. |
| [`Mod.Default`](src/Mod/Default.elm) / [`Mod.Hard`](src/Mod/Hard.elm) | Two content mods. |
| [`Main`](src/Main.elm) | Wiring: input, the mod/renderer toolbar, `Browser.element`. |

## Writing a mod

**New content:** copy [`Mod/Default.elm`](src/Mod/Default.elm), change the numbers / add `EnemyDef`s
and `ItemDef`s, and register your `Ruleset` in the `mods` list in [`Main`](src/Main.elm). Done — the
engine plays it.

**New renderer:** write a module exposing `renderer : Renderer msg` whose `view` draws a `Scene` (see
[`Render/Ascii.elm`](src/Rogue/Render/Ascii.elm) for a compact example), and add it to the
`renderers` list in `Main`.

## Status

- **Set 1 — playable moddable core:** generation, FOV, bump combat, monster AI, items, depth/stairs,
  win/lose, two mods, two renderers.
- **Set 2 — depth:** equipment, character classes, regional level themes, traps + search, BFS/ranged/
  fleeing AI, status effects, XP & leveling, potion identification, scrolls & wands, and `localStorage`
  run history.

- **Set 3 — Pixel-Dungeon depth:** locked vaults + keys, special rooms, secret/open doors, monster
  abilities (split/steal/regenerate), boss floors, the **Amulet of Yendor** win, equipment
  enchantment, thrown attacks, hunger, dark floors + torches, paralysis/haste/slow, wandering spawns,
  shops, an inventory/character screen, **camera-follow viewport culling**, a minimap, floating combat
  numbers, custom-seed/daily runs, a bestiary journal, and a **32-test headless suite**.

Every system above is data behind the same two seams — content is a `Ruleset`, rendering is a
`Renderer` — so mods extend all of it. Run the tests with `ELM=../../elm.sh ./test.sh`; see
[PERFORMANCE.md](PERFORMANCE.md) for the rendering/speed analysis. Iterating toward Pixel-Dungeon
parity.
