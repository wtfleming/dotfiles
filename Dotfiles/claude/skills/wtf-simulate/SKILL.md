---
name: wtf-simulate
description: >-
  Build an interactive, self-narrating explainer that teaches a complex
  technical system or process — a real simulation underneath, station-by-station
  narration on top, in one dependency-free static site (HTML + canvas 2D + plain
  JS, no build step, no framework). Two forms: an isometric town a vehicle
  drives through (the RollerCoaster-Tycoon-style tour), and a flat 2D schematic
  with a focus ring for architectures where many things move at once. Use when
  the user wants to teach or visualise how something works end to end and a
  chart or a diagram is not enough: "explain how X works visually", "build an
  interactive/animated explainer for X", "visualise the whole
  pipeline/architecture", "RollerCoaster Tycoon style", "a city/factory/town
  that shows how X works", "explorable explanation", "learning simulation".
  Also use when editing an existing project built from this skill — look for a
  stations table in world.js and a sim.js pacing state machine. SKIP for: a
  single chart, graph or plot (use dataviz); a whole dashboard (use
  wtf-dashboard-design); a quick sketch of a
  shape — a call tree, a file tree, a diff of either, or a diagram stepped
  through with nothing computed (use wtf-show-me); a
  static diagram or flowchart (use artifact-diagramming); a slide deck; a game
  with win conditions and scoring; a 3D scene needing WebGL or real
  perspective; a UI mockup.
---

# Interactive explainers

You are building a **place that runs a mechanism**. Something moves through it
carrying real state; each stop is one step of the real thing; the tour pauses
long enough for the reader to read about the step it just performed.

This works because it turns an invisible process into a journey with
geography. The reader gets sequence for free (the road), scale for free (the
distance), proportion for free (the size of a building), and repetition for
free (a loop in the road is a loop in the algorithm). Nothing here is
decoration hung on a lecture — the animation *is* the explanation.

Read this file, then read `references/build-order.md` and follow it. The other
references are lookups for when you reach the part they cover.

## The five rules

Everything else in this skill follows from these. Break one and you get a
screen saver with captions.

1. **Compute the real thing.** `model.js` must actually do the work — the real
   arithmetic, the real algorithm, at reduced scale. Never interpolate between
   pre-baked results. When the reader drags a slider the numbers must move
   because the model moved.
2. **The moving element carries state, not cargo.** The bars on the vehicle's
   flank are the actual vector. The crates in its bed are the actual byte
   count. A token's size is the actual message size. If you could delete the
   markings without losing information, they were decoration.
3. **Publish a fidelity ledger.** Say exactly what is computed, what is scaled
   down, what is assumed, and what is faked — in the About modal, the README,
   the panel and the code. Half the educational value is the reader knowing
   which numbers to trust. See `references/fidelity.md`.
4. **Pace it for reading, not watching.** The first visit to a station stops
   for as long as its write-up takes to read. Later visits get a beat. The
   reader never has to pause to keep up. See `references/pacing.md`.
5. **One canvas, no dependencies.** Canvas 2D, plain ES5-style JS in IIFEs,
   `<script>` tags in order, no build step, no network calls. It opens from
   `file://`, it works offline, it deploys to any static host, and it is still
   readable in five years.

## Choosing a form

Two forms, one decision: **what travels?**

- **Isometric tour** — one unit of state moves through the whole process: a
  request, a part under construction, a hidden state, a build artifact. This
  is the default, the form the template implements, and the low-poly
  tycoon-game look people ask for by name. Pipelines, protocols-as-journeys,
  manufacturing processes, request lifecycles.
- **Flat schematic** — no single traveller: an architecture where many things
  move at once, a negotiation between peers, a topology. A focus ring tours
  the stations while the system visibly keeps running. See
  `references/flat-format.md`.

If nothing is computed, stop — the user wants a diagram, not this skill.
That is `wtf-show-me` for a quick sketch of a shape or a diagram stepped
through a part at a time, `artifact-diagramming` for a finished static diagram.

## Layout

Nine files, in dependency order. Keep these names and this split — it is the
same shape in every project built from this skill, and the whole skill assumes
it. (The flat form drops `iso.js`, leaving eight; see
`references/flat-format.md`.)

```
index.html          markup, controls, the About modal with the fidelity ledger
css/styles.css      light print-like UI: full-screen canvas, floating panels
js/iso.js           ENGINE. Projection, solids, routes. Copy unchanged.
js/model.js         THE LESSON. The real simulation. Write this first.
js/world.js         the static place: routes, stations, districts, buildings
js/sim.js           mostly ENGINE. State machine: travel, stations, reading stops.
js/render.js        one sorted painter's pass over everything with a footprint
js/ui.js            DOM panels, live numbers, narration, controls
js/main.js          ENGINE. Camera, input, frame loop. Copy nearly unchanged.
```

`iso.js` and `main.js` are engine: copy them from `assets/template/js/` and
change almost nothing. `sim.js` is engine too, except where it names the
domain; its header lists those parts. They are yours, and the rest of the file
stays as it is. `model.js` and `world.js` are 100% yours. `render.js` keeps
its structure — the sorted pass, the label pass — and gets new landmark
functions. `ui.js` keeps its shape and gets your panels.

## Start here

Copy the working template, then replace the domain:

```bash
cp -r <skill-dir>/assets/template my-explainer
cd my-explainer && python3 -m http.server --bind 127.0.0.1 8000
```

The template is **PacketPost**, a complete small explainer of what a web
request costs: eight stations, a real latency model with TCP slow start, a
branch in the road for a CDN cache hit. It runs. Open it before you change
anything, watch one full trip, and notice what the pacing feels like — you are
trying to reproduce that feeling, not that subject.

Then work through `references/build-order.md`. Do not start with the scenery.

## Verify before you claim it works

A canvas app fails silently: one thrown error and you get an empty frame with
a clean-looking page. Always run this before reporting done:

```bash
# syntax — a loop, because `node --check` checks only its first file argument
fail=0; for f in js/*.js; do node --check "$f" || fail=1; done; test "$fail" -eq 0

# console errors, every station, screenshots. Bind loopback and kill the server
# after: a stale listener on the port makes the smoke test pass against another
# project. The trap does the killing, so the exit status is the smoke test's.
python3 -m http.server --bind 127.0.0.1 8000 & pid=$!
trap 'kill "$pid" 2>/dev/null' EXIT
sleep 1
if kill -0 "$pid" 2>/dev/null; then
  node ~/.claude/skills/wtf-simulate/scripts/smoke.mjs http://127.0.0.1:8000/
else
  echo "FAIL: server did not start (port 8000 busy?)" >&2
  false
fi
```

`scripts/smoke.mjs` loads the page in headless Chromium, fails on any console
error or page error, steps the vehicle through every station, and writes a
screenshot at each one plus one of the finished run. **Look at the
screenshots.** Occlusion, label collisions and plates landing on empty ground
do not raise errors.

`smoke.mjs` needs Playwright (`npm i -D playwright` in the project). If it is
not installed, do the same checks by hand with the chrome-devtools MCP tools:
navigate to the served URL, read the console messages (must be empty), step
the sim with `evaluate_script` (`Sim.state.speed = 8; Sim.step()`) until
`Sim.state.finished`, taking a screenshot at each station — and look at
them. Never report a project as working on `node --check` alone.

## References

| File | Read it when |
|---|---|
| `references/build-order.md` | before writing anything — the phases, in order |
| `references/flat-format.md` | building the flat schematic form |
| `references/fidelity.md` | writing the About modal, the README, or any number |
| `references/pacing.md` | tuning stops, fast-forward, or the tour-done switch |
| `references/isometric-drawing.md` | drawing anything isometric: projection, sort order, occlusion, faces |
| `references/narration.md` | writing district copy and panel text |
| `references/checklist.md` | before you call it finished |

---

Adapted from [learnscape](https://github.com/LaurentiuGabriel/learnscape) by
Laurentiu Gabriel (MIT — see `LICENSE`). The template is vendored from
upstream **with local fixes** — `main.js`, `sim.js`, `ui.js`, `index.html`
and the template README have diverged where review found defects, so do not
"refresh" them from upstream: that silently reverts the fixes. The skill
directory's git history is the divergence record.
