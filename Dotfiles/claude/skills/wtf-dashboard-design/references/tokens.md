# Tokens

The components and snippets name colours by role, not by value. Map each role to a value from the project's own
design system. If the project has none, take the values from the `dataviz` skill's palette. Define every role once
for light and once for dark, then make tints from token + alpha (`bg-accent/10`), so a tint adapts to the theme by
itself.

| Token | Role |
|---|---|
| `page` | app background |
| `card` | panels |
| `soft` | hover, segmented track |
| `side` | sidebar |
| `ink` | primary text |
| `ink-2` | secondary text, unselected values |
| `muted` | labels, axes; ≥ 4.5:1 on every surface it sits on |
| `line` | hairlines, bar tracks (ink at ~7.5%) |
| `line-strong` | input borders, at ≥ 3:1 against the surface they sit on; disabled text |
| `accent` | traffic and people metrics, selection, focus |
| `on-accent` | text on a solid accent fill |
| `money` | revenue metrics |
| `up` | good change |
| `down` | bad change on a meaningful base |
| `warn` | below threshold: heatmap warn tier, overdue chips |
| `tip` | tooltip surface |
| `live` | live dots |

One non-colour token goes with them: `--shadow-card`, a faint card shadow
(`0 1px 2px rgb(0 0 0 / .04)`), which Tailwind v4 also reads from `@theme`.

In Tailwind v4, declare them as `--color-<token>` under `@theme`; in plain CSS, as custom properties. Put the dark
values under `@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) { … } }` and again under
`:root[data-theme="dark"]`, so a theme the app pins wins over the system setting. Or declare each token once with
`light-dark()` and let `color-scheme` decide.

## Rules

- Two semantic hues carry meaning: `accent` (traffic, people, selection) and `money` (revenue), plus `warn` for
  "below threshold". Everything else is ink, muted or line. Another hue needs a reason (a third entity), not
  decoration.
- Three intensities, three jobs (Few): **pale** (token at 7–20%) for non-data and de-emphasis: tracks, bar fills,
  healthy heatmap tiers, avatar tints; **medium** (the token) for data marks; **bright or dark** only for what needs
  attention. A screen of medium and bright everywhere has nothing left to highlight with.
- Heatmap tiers are token + alpha, so they adapt to both themes: `accent` at 25 / 15 / 7% over the card for the
  healthy steps, `warn` at 30% with dark text for the problem tier.
- Tints are token + alpha: selection `bg-accent/[.06] ring-accent/20`, range band `bg-accent/10`, bar track
  `bg-line/70`, hover `bg-soft/70`.
- Check muted text contrast on every surface it sits on (card, soft, hover, page), in both themes.
- A theme setting inside the app overrides the system scheme: test dark mode by setting the app's theme too.

## Density scale

Base **14 px** text (13 px for dense tables and sub-lines), **16 px** icons, stroke **1 / 1.2 / 1.5** (1 in nav
lists, 1.2 default, 1.5 selected or alone), weights regular and medium, semibold only for the page title or a
headline number. A 13 px base is fine if it is held everywhere. Never mix stroke widths inside one icon row.
