# The flat schematic form

The second form this skill builds. Same lesson machinery — a real model, a
station tour paced for reading, a fidelity ledger — drawn as a flat 2D
schematic instead of an isometric town: nodes for the parts of the system,
lanes and edges for the paths between them, animated tokens for whatever
moves.

## What replaces the vehicle

The tour survives; the vehicle becomes a **focus ring**. Stations are still
visited in lesson order, each first visit still stops for `readSeconds()`,
and the write-up rules in `narration.md` apply unchanged. While the focus
ring sits on a station, the tokens elsewhere keep moving at an idle pace —
the system visibly stays alive — but only the focused station's numbers are
narrated. The reader can only read one thing.

Rule 2 still binds: the tokens carry real state, not decoration. A message
token's size is the actual byte count; a queue's depth is the actual queue
depth from `model.js`. If deleting a token's markings loses no information,
it was decoration.

## File split

Identical to the isometric split minus `js/iso.js`. Keep the interfaces
`sim.js` expects, and it copies over nearly unchanged — the pacing state
machine neither knows nor cares that there is no projection:

- routes are still polylines with a `total` and `cum[i]`, just in plain 2D
  world coordinates; stations still anchor to waypoint indices;
- `World.readSeconds` and `World.stationToDistrict` keep their jobs;
- `vanPosition()` becomes the focus-ring position along the tour route — it
  is the one place `sim.js` calls `Iso`.

Keep the globals as well — `World`, `Sim`, `Renderer`, `UI` — and
`scripts/smoke.mjs` runs against the flat form unchanged.

`render.js` becomes simpler, not different in kind. There is no sorted pass;
draw in fixed order — background, lanes and edges, tokens, nodes, labels,
overlays. Token motion along an edge is position by distance, exactly like
the van, never a CSS-style tween between endpoints.

These sections of `isometric-drawing.md` still apply verbatim:

- **Labels** — screen space under the dpr transform, de-collision, declutter.
- **Determinism** — variation from `hash2(x, y, s)`, never `Math.random()`
  in a draw call.
- **Camera** — start close on the focus, frame-rate-independent easing, aim
  at the DOM-measured visible rectangle, drag turns follow off.

## Checklist substitutions

Walk `checklist.md` as usual, replacing its Rendering section with:

- [ ] Draw order is fixed and layered; no token ever paints over a label.
- [ ] Tokens move by distance along an edge, not by tweening endpoints.
- [ ] Idle motion continues during a reading stop, visibly slower.
- [ ] The focused station is unmistakable — ring, dimmed neighbours, or both.
- [ ] Labels drawn with the **dpr** transform, verified at
      `deviceScaleFactor: 2`.
- [ ] No `Math.random()` in any draw path.
- [ ] Camera easing is frame-rate independent; a drag turns follow off.
- [ ] Fitting the whole schematic on screen shows the shape of the system —
      the topology is legible with the labels off.
