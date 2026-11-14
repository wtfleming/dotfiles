# Finishing checklist

Walk this before calling an explainer done. It covers what your own work can
get wrong; what the engine files already do is not re-checked here.

Everything here applies to both forms except **Rendering**, which assumes the
isometric form — for the flat schematic form, substitute the checks at the end
of `references/flat-format.md`.

## The model

- [ ] `model.js` computes real values from inputs — no lookup tables, no lerps
      between pre-baked results.
- [ ] It runs standalone in `node` and produces sane numbers.
- [ ] Every displayed number traces to a file and a line.
- [ ] Every assumption is marked `// ASSUMED` at its point of definition.
- [ ] Every slider changes a real model parameter, and takes effect immediately.
- [ ] Dragging a slider mid-run changes the stations not yet reached, and does not
      retroactively rewrite the ones already charged.

## The journey

- [ ] Every model step has a station; every station has a district.
- [ ] Repetition is a loop in the road, not a row of buildings.
- [ ] Branching is a junction the vehicle visibly turns at.
- [ ] The vehicle carries real state — its markings are data, not decoration.
- [ ] Stations are anchored to waypoint indices, not hand-measured distances.
- [ ] All route changes live in `advanceRoute()`.

## Pacing

- [ ] Stations that share a write-up are mapped in `stationToDistrict`, so it is
      not charged twice.
- [ ] A progress bar shows the remaining stop time.
- [ ] The HUD says which pacing mode is active, always.
- [ ] The first full pass takes roughly 4–6 minutes at 1×.

## Rendering

- [ ] Structures the road passes under are registered as separate pieces.
- [ ] No building violates `(w + d) / 2 > setback` against the route.
- [ ] Flat layers (ground, washes, roads, elevated pillars) painted before the
      sorted pass; overlays after it.
- [ ] No `Math.random()` in any draw path.
- [ ] Face decoration uses `FACE_ANG` / `FACE_U`, not guessed pixel offsets.

## Copy

- [ ] Every district has `tag`, `short`, `body`.
- [ ] Every `body` is 60–110 words and contains at least one number.
- [ ] Every `body` says why the stage exists, not only what it does.
- [ ] At least one district points at something visible on screen.
- [ ] At least one district invites a slider experiment and predicts the outcome.
- [ ] The reader is told what to trust and what to ignore.
- [ ] The done card lands the lesson rather than saying "complete".
- [ ] Read the `short`s in sequence — coherent summary of the whole system.
- [ ] A reader who knows the domain would find nothing to disagree with; anything
      they would is either wrong or belongs in the ledger.
- [ ] Read aloud once. Awkwardness you skim past on screen is obvious out loud.

## The fidelity ledger

- [ ] In the About modal, headed "How much of it is real".
- [ ] In the README, with source files named.
- [ ] Buckets: computed / scaled down / assumed / faked, each specific.
- [ ] Estimates flagged inline in the panel, not only in the modal.
- [ ] Anything faked is named, with the reason, and the reader is told where to
      point their attention instead.

## Shell and platform

- [ ] Opens from `file://` with no server.
- [ ] No network requests, no CDN, no fonts fetched, no build step.
- [ ] Works at 360 px wide: panel becomes a bottom sheet, sliders behind the gear.
- [ ] **Landscape phone** checked — the worst case, and the easiest to forget.
- [ ] Buttons have `title` and `aria-label`; toggles are real labelled checkboxes.

## Verification

- [ ] The `node --check` loop from `SKILL.md` passes.
- [ ] `scripts/smoke.mjs` reports zero console errors and zero page errors.
- [ ] The smoke test reaches every station and finishes the run.
- [ ] **Every station screenshot has been looked at.** Occlusion and label
      collisions raise no errors.
- [ ] One full first-visit tour watched at 1×, start to finish, without touching
      anything.
- [ ] README station table matches the actual stations.
