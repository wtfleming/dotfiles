# HTML page design

Read this before writing the HTML file from `SKILL.md`'s last section. It
covers what makes a drawn page look designed rather than generated: how much
goes in, the palette and type, and the rules connectors follow. Where the
subject has a product UI of its own, its colours and type win over the tokens
below; everything else here still applies.

## Delete first

The best edit is usually a removal. Two nodes that always travel together are
one node. An arrow whose relationship is obvious from the layout comes out.
The page is done when nothing can be removed, not when everything is in.

| Per diagram | Limit |
|---|---|
| Nodes | 9 |
| Arrows | 12 |
| Accent-coloured elements (an arrow into an accented box is part of it) | 2 |
| Italic callouts | 2 |

Over budget, split into an overview and a detail — two small diagrams beat one
dense one.

## Palette

Name colours by role, never by hex, and switch with the system theme. Every
text role clears 4.5:1 against `--paper` in both themes; keep it that way if a
value changes.

```css
:root {
  --paper: #f5f5f5; --paper-2: #ececec; --ink: #2d3142;
  --muted: #4f5d75; --soft: #5a6580; --rule: rgba(45,49,66,.12);
  --accent: #bf4520; --accent-tint: rgba(191,69,32,.08); --link: #2e5aa8;
}
@media (prefers-color-scheme: dark) {
  :root {
    --paper: #2d3142; --paper-2: #393e53; --ink: #f5f5f5;
    --muted: #bfc0c0; --soft: #949eb2; --rule: rgba(245,245,245,.12);
    --accent: #f08a59; --accent-tint: rgba(240,138,89,.10); --link: #739fdf;
  }
}
```

- `--accent` marks the one or two things the page is about. Accenting four
  means nothing has been chosen as the focus yet.
- `--muted` is the default arrow; `--link` is a call across a boundary
  (HTTP, an external service); dashed `5,4` is optional, async or a return.
- A translucent fill lowers the contrast of text on it. Measure text on
  `--accent-tint` or `--paper-2` against that surface.

## Type

System stacks only — the page opens from `file://` with no network.

| Role | Stack | Size | Use |
|---|---|---|---|
| title | `ui-serif, Georgia, serif` | ~1.75rem | the page `h1` |
| name | `system-ui, sans-serif`, 600 | 12px | human-readable node names |
| sublabel | `ui-monospace, Menlo, monospace` | 9px | paths, commands, ports, line numbers |
| eyebrow | mono, uppercase, tracked .18em | 7–8px | type tags, section labels |
| callout | serif *italic* | 14px | an editorial aside, at most two |

Sizes in px are viewBox units inside an SVG; in page CSS they are CSS px.

Mono is for technical content, not a blanket "dev" look: a node called
"Copy emacs packages" is sans, its `wtf-elisp/*.el` sublabel is mono. Never put
a command or path in an eyebrow — its uppercase turns `set -e` into `SET -E`.

Size a box from its label, not by eye: about 0.60em a character for sans and
0.62em for mono, plus 8px of padding. System faces differ in width from
machine to machine, and a box fitted to one clips its label on another.

## SVG connectors

1. **Right angles only.** An off-axis connector is an elbow with an `r=8`
   rounded corner; a straight line only when both ends share x or y. It leaves
   and enters a box perpendicular to the edge, at least 8px from a corner.
2. **Labels sit beside the line, not on it.** At most 14 characters, uppercase,
   on an opaque `--paper` rect with a visible 6–10px gap to the stroke — beside
   a vertical segment, above a horizontal one.
3. **No shared strokes.** Parallel routes stay 12px or more apart; a single
   crossing gets a small hop.
4. **One attach point per connector.** N connectors on an edge of length L sit
   at `L * k / (N + 1)`, 12px or more apart.
5. **Route around boxes that are not an endpoint.** Only when no route exists,
   dash the stroke and put the label at the visible end.
6. **Draw arrows, then label masks, then boxes.** Boxes start with an opaque
   `--paper` rect so a line behind them stays hidden, and a label mask must not
   overlap a box painted after it.

Boxes take `rx` 6 at most, a 1px border and no shadow. A type tag in a box is a
small `rx=2` rectangle, not a pill.

## Showing a change

When a `diff` fence cannot hold a structural change, draw Before · Changes ·
After: two topology panels of the same size with a short ledger between them.
If only attributes changed and the boxes and arrows did not, it is a table, not
two diagrams.

- Copy Before's coordinates into After, and move a box only when the move is
  part of the change. Two panels laid out separately make everything look moved.
- Mark each change with a word and a shape, never colour alone: a `+` badge for
  added, `−` and a dashed outline for removed, `Δ` for changed. Unchanged boxes
  stay plain.
- Each ledger line starts with the change word, names the object and says what
  differs — `CHANGED orders: dispatch → enqueue`, never just "updated".
- The budget counts a box that appears in both panels once.

## Anti-patterns

| Looks like | Why it fails |
|---|---|
| Dark background with cyan or purple glow | Looks technical without a design decision |
| Every node the same box | Erases hierarchy |
| Legend floating inside the drawing | Collides with nodes — use a strip along the bottom |
| Vertical `writing-mode` text | Unreadable |
| Three equal-width cards | A template, not a layout — vary widths (`1.1fr 1fr .9fr`) |
| `display: block` on `code` inside prose | Breaks a sentence into fragments, one per code span |
| Mermaid's auto-layout reproduced by hand | Keeps its spacing and routing, loses the editorial choice |

## Accessibility and phones

- Each `<svg>` gets `role="img"` and `aria-labelledby` naming a `<title>` (its
  first child) and a one-sentence `<desc>` of the content, not the geometry.
  Prefix their ids with the diagram's slug.
- A wide SVG goes in an `overflow-x: auto` wrapper, and the SVG itself takes
  `min-width` equal to its viewBox width, so a phone scrolls the diagram rather
  than the page. On the wrapper, that `min-width` widens the page instead.

## Look before handing it over

No tool checks the connector rules, so check them by eye on a render — every
label gap, attach point and crossing:

```sh
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new \
  --hide-scrollbars --window-size=1280,1400 \
  --screenshot=<scratch>/show-me/{topic}.png file://<scratch>/show-me/{topic}.html
```

Read the PNG, fix what it shows, then `open` the HTML.

Check phone fit by measurement, not on the screenshot: a crop at the right edge
looks like the page ending there. Render a probed copy at 500px, the narrowest
window headless Chrome allows; it prints `innerWidth scrollWidth`, and the two
must match:

```sh
{ cat <scratch>/show-me/{topic}.html
  echo '<script>addEventListener("load",()=>document.title=innerWidth+" "+document.documentElement.scrollWidth)</script>'
} > <scratch>/show-me/{topic}-probe.html
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new \
  --window-size=500,900 --dump-dom file://<scratch>/show-me/{topic}-probe.html \
  2>/dev/null | grep -o '<title>[^<]*'
```

A phone is narrower still, so any fixed width not inside a scroll wrapper must
stay under 360px. Inside one, a width can be anything — the wrapper scrolls.

---

The budget, palette, type roles, label widths, connector rules, the
before/after layout and most anti-patterns are condensed from [cathrynlavery/diagram-design](https://github.com/cathrynlavery/diagram-design)
(MIT — see `LICENSE-diagram-design`), with its web fonts replaced by system
stacks to keep the page offline.
