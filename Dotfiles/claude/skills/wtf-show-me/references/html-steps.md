# Stepping through a diagram

Read this when the point of a page is an order — a request's path, a state
machine's transitions, a migration's stages — and seeing it all at once hides
which came first. Next and Back reveal the diagram a step at a time, each with
a one-line caption. Nothing is computed: every step is already drawn. If what a
step shows has to be worked out from the reader's input, that is
`wtf-simulate`.

## Rules

- **Complete without the script.** Every node, arrow and label is in the markup.
  Only the `.steps-ready` class the script adds last dims anything, so a script
  error leaves the whole diagram and every caption showing. Print does the same.
- **Mark parts by step.** `data-step="N"` on a `<g>`, numbered from 1, at most
  two parts entering per step. Aim for 3–6 steps; past 8, split the diagram.
- **One caption per step**, in an `<ol>` with one `<li>` per step. Without the
  script it reads as a numbered list beside the full diagram; with it, only the
  current line shows.
- **Reveal, never move.** Opacity only — no change to coordinates, routes, the
  `viewBox` or any text. Earlier steps stay at full strength.
- **The reader paces it.** No autoplay, no timers. Under
  `prefers-reduced-motion` stepping still works; only the fade goes.
- The SVG's `<desc>` describes the whole diagram, not the first step.

## Markup

```html
<figure class="stepper" data-steps="3">
  <svg role="img" aria-labelledby="flow-title flow-desc" viewBox="0 0 640 200">
    <title id="flow-title">…</title><desc id="flow-desc">…</desc>
    <g data-step="1">…</g>
    <g data-step="2">…</g>
    <g data-step="3">…</g>
  </svg>
  <ol class="captions"><li>…</li><li>…</li><li>…</li></ol>
  <div class="controls" hidden>
    <button type="button" data-go="-1">Back</button>
    <span class="status" aria-live="polite"></span>
    <button type="button" data-go="1">Next</button>
    <button type="button" data-go="0">Start over</button>
  </div>
</figure>
```

```css
.steps-ready [data-step] { opacity: .12; transition: opacity .3s; }
.steps-ready [data-step].shown { opacity: 1; }
.steps-ready .captions li { display: none; }
.steps-ready .captions li.now { display: list-item; }
@media (prefers-reduced-motion: reduce) { .steps-ready [data-step] { transition: none; } }
@media print {
  .steps-ready [data-step] { opacity: 1; }
  .steps-ready .captions li { display: list-item; }
  .controls { display: none; }
}
```

```js
document.querySelectorAll('.stepper').forEach(function (fig) {
  var n = Number(fig.dataset.steps), at = 1;
  var parts = fig.querySelectorAll('[data-step]');
  var caps = fig.querySelectorAll('.captions li');
  var controls = fig.querySelector('.controls');
  var back = controls.querySelector('[data-go="-1"]');
  var next = controls.querySelector('[data-go="1"]');
  function show(k) {
    at = Math.max(1, Math.min(n, k));
    parts.forEach(function (p) { p.classList.toggle('shown', Number(p.dataset.step) <= at); });
    caps.forEach(function (c, i) { c.classList.toggle('now', i === at - 1); });
    back.disabled = at === 1;
    next.disabled = at === n;
    controls.querySelector('.status').textContent = 'Step ' + at + ' of ' + n;
  }
  controls.addEventListener('click', function (e) {
    var b = e.target.closest('[data-go]');
    if (b) show(b.dataset.go === '0' ? 1 : at + Number(b.dataset.go));
  });
  show(1);
  controls.hidden = false;
  fig.classList.add('steps-ready');
});
```

`data-steps` and the number of captions must both equal the highest
`data-step`. The page still opens from `file://`; the script is inline.

---

Condensed from the `step` mode in
[cathrynlavery/diagram-design](https://github.com/cathrynlavery/diagram-design)'s
`references/animation.md` (MIT — see `LICENSE-diagram-design`). Its autoplay,
loop and timing modes are left out, and the controller is local.
