---
name: wtf-dashboard-design
description: >-
  Design, build or critique a whole dashboard screen on the web: what question it answers, the KPI
  tiles, the page layout at ultrawide, laptop and phone widths, comparison periods, ranked breakdowns,
  date range pickers, filters, the loading, stale, empty and error states, and a QA pass that checks
  the numbers agree before screenshots are taken. Use when building, redesigning or critiquing an
  analytics, revenue, metrics or admin dashboard, a stats page or any data-heavy screen: "build a
  dashboard for", "redesign this dashboard", "is this dashboard any good", "/wtf-dashboard-design".
  NOT for a single chart or a colour palette on its own (that is dataviz), or a page that is not
  mostly data.
---

# Dashboard design

A dashboard answers **one question per screen at first glance** ("is the school OK today?", "is it growing, and
what's driving it?"), shows what needs attention, and puts the action one click away. Every element answers the
question, flags a problem, or gets out of the way.

This skill owns the screen: composition, components, states and QA. For the marks inside a single chart and for
colour values, the `dataviz` skill is the authority; where the two disagree on a chart detail, follow `dataviz`.
A project's own design system wins over both, and a taste the user states wins over all three. Note at the top of
the work which one you followed.

## Leading words

- **First glance**: what a reader gets in 3 seconds without hovering. Design for it first.
- **Monitor**: the reader's sequence: overview → what needs attention → closer look → act. Problems are found in a
  glance, and each has its action beside it.
- **Quiet**: calm until something is wrong. Grey and one entity hue for healthy data; bright or warm colour only on
  what needs attention.
- **Wordless**: names, numbers, units and one- or two-word baselines on the screen, plus compact numeric facts
  ("CPL 24 902 · CTR 1.8%"); every explanation lives in a tooltip. Zero sentences outside tooltips and empty states.
- **Honest**: a number is the same everywhere it appears, estimates are labelled, a tiny base never shouts.
- **Goal beside the number**: a target metric shows value, goal and what is left on one tile (tick bar, plain words).

The perception behind these words (encoding ladder, colour rules, scales, bullet graphs, accessibility):
[references/perception.md](references/perception.md).

## Build or redesign

1. **Name the question.** Write the screen's one question, the 3–6 numbers that answer it, the problem rules as
   numbers ("CPL above 1.2× target on 2 of the last 3 days", "attendance below 85%"), and the action for each
   problem. Seed a fixed fixture that triggers every problem and write its expected answers now, before any UI
   (qa.md → Known totals). Done when the four lists, the fixture and the expected answers exist.
2. **Pick a form for each number** from the encoding ladder (perception.md): position or length for quantity, a
   heatmap only for a grid, a bullet graph against a target, a sparkline for history. Done when every number has a
   form and a one-line reason.
3. **Lay out the hierarchy**: header (title, range, filter) → KPI cards → the main view (the answer, and for
   monitoring the grid or list where problems show) → breakdowns → detail lists. Most important top-left; cards in a
   row hold similar amounts of content; on a monitoring screen the KPI cards and the problem view fit above the
   fold. Widths, grids, phones: [references/layout.md](references/layout.md). Done when the eye has one obvious path
   and no card is half empty.
4. **Set the tokens** before any component: [references/tokens.md](references/tokens.md). Done when every colour
   in the UI is a token with a light and a dark value.
5. **Build each component from its pattern** in [references/components.md](references/components.md), reusing the
   code in [references/snippets.md](references/snippets.md) where it fits. Snippets are React + Tailwind; on plain
   HTML, translate the classes to CSS on the same tokens. Done when every component follows its pattern or its
   deviation is written in a short comment block at the top of the page's file.
6. **Design every state**: loading, stale (old data while new loads), error, empty, tiny data, sample data
   (components.md → States). Done when each panel has been seen in each state.
7. **Run the QA pass**: [references/qa.md](references/qa.md). It starts with known totals, then the words audit and
   the salience test, then screenshots at three widths. Done when the numbers match, the sentence count is zero, the
   eye lands on the problem, and every check passes on screenshots you actually looked at.

## Critique ("is this a good design?")

Look at the screen (screenshot or live), then answer in this order:

1. **Verdict** in one line, plain ("Better, not good yet").
2. **What works**: 2–4 specifics.
3. **Issues ranked by harm**: honesty (numbers that disagree, misleading deltas or sparklines) → salience (does the
   eye land on what needs action, or is the problem the palest thing?) → hierarchy (what leads, what competes) →
   text (sentences that should be shape or tooltip) → component smells (qa.md smell table) → polish. Each issue names
   the element, what the reader sees, and the fix.
4. Offer to fix them in one pass, screenshots before commit.

Every critique finds the honesty problems first; a pretty screen with two answers to one question is not good.
A screen that is honest but reads as a report (sentences under numbers, a row of insight sentences) fails the
Wordless test; qa.md → Words audit says where each sentence goes instead.

## Rules that apply everywhere

- **Colour follows the entity**: one hue for people or traffic, one for money; a warm hue only for a problem; red
  only for a bad change on a meaningful base; intensity grows toward the problem, never toward "good". Unselected
  tiles, previous periods and secondary series are grey. Red and green never carry meaning alone.
- **Context**: every headline number answers "compared to what?" with one baseline: previous period (same elapsed
  time), target, normal range or history.
- **Numbers**: tabular figures, right-aligned in columns; as precise as the decision needs (5.2M on a card, exact in
  the tooltip); compact on axes (1k, $4k); `<0.1%` below 0.1%; `—` for no data, never a fake zero amount or 0%.
- **Comparison**: the previous period is a grey or dashed line; an unfinished period is drawn dashed. A base under
  ~5 reads "was 1" in grey, never a red −100%. Deltas are coloured by meaning: lower-is-better metrics (cost, CPL,
  churn) flip green and red; neutral ones (spend, volume) stay grey.
- **Say it once**: a number shown twice agrees, and appears once as a claim.
- **Attention lives on the data**: the problem is flagged where it appears (a warm heatmap cell, a red chip) with
  its action beside it, not restated in a sentence elsewhere.
- **Selection** is a soft tinted fill with a faint ring.
- **Share bars** are slim 3 px rounded tracks along the row's bottom edge.
- **Controls are custom and calm**: a popover calendar for dates; a menu grid for phone navigation; segmented tabs
  that shrink rather than scroll.
- **Density**: 14 px base text, 16 px icons, 1 / 1.2 / 1.5 stroke, medium weight over semibold ([tokens.md](references/tokens.md)).
- **Width grows with the screen** on dashboards: more columns, never a narrow column floating in white space.
