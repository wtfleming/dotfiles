/* kit.js — pure-of-t helpers for a seek(t) film. Every function here returns or
   paints the same thing for the same t, whatever order frames are asked for in. */

const clamp = (x, a = 0, b = 1) => Math.min(b, Math.max(a, x));
const lerp = (a, b, k) => a + (b - a) * k;
/* 0 before start, 1 after end, linear between. */
const span = (t, start, end) => (end > start ? clamp((t - start) / (end - start)) : +(t >= end));

/* Closed-form damped spring from 0 to 1, started at t=0. period is the
   undamped oscillation in seconds; zeta < 1 overshoots, zeta = 1 does not.
   It returns exactly 1 once what is left of the motion is under 0.1% of the
   travel: a spring otherwise never arrives, and a film that ends on one can
   never match its own first frame. */
function spring(t, { period = 0.5, zeta = 0.8 } = {}) {
  if (t <= 0) return 0;
  const w = (2 * Math.PI) / period;
  if (zeta >= 1) {
    const left = Math.exp(-w * t) * (1 + w * t);
    return left < 1e-3 ? 1 : 1 - left;
  }
  const wd = w * Math.sqrt(1 - zeta * zeta);
  // Bounds the oscillation's amplitude from above, so it never cuts off a swing.
  if (Math.exp(-zeta * w * t) / Math.sqrt(1 - zeta * zeta) < 1e-3) return 1;
  return 1 - Math.exp(-zeta * w * t) * (Math.cos(wd * t) + (zeta * w / wd) * Math.sin(wd * t));
}
const SPRING = {
  ui:      { period: 0.35, zeta: 0.72 }, // buttons, cards: a tiny overshoot
  camera:  { period: 0.6,  zeta: 0.9 },
  type:    { period: 0.55, zeta: 1 },    // text never overshoots
  playful: { period: 0.5,  zeta: 0.45 },
};

/* A value that is retargeted over time: [[t0, v0], [t1, v1], ...]. One spring
   per change, each starting at its own time, so motion stays continuous and
   seek(t) never needs the frames before it. */
function track(t, keys, preset = SPRING.ui) {
  let v = keys[0][1];
  for (let i = 1; i < keys.length; i++) v += (keys[i][1] - keys[i - 1][1]) * spring(t - keys[i][0], preset);
  return v;
}

/* Sizes derive from the frame, never fixed pixels, so one timeline renders at
   16:9, 9:16 and 1:1. u is one pixel of a 1080-tall (or -wide) frame. */
function layout() {
  const W = innerWidth, H = innerHeight;
  return { W, H, cx: W / 2, cy: H / 2, u: Math.min(W, H) / 1080, portrait: H > W };
}

function place(el, { x, y, w, h, scale = 1, opacity = 1, radius }) {
  if (w != null) el.style.width = w + 'px';
  if (h != null) el.style.height = h + 'px';
  if (radius != null) el.style.borderRadius = radius + 'px';
  const ew = w ?? el.offsetWidth, eh = h ?? el.offsetHeight; // unaffected by transform
  el.style.transform = `translate(${x - ew / 2}px, ${y - eh / 2}px) scale(${scale})`;
  el.style.opacity = opacity;
}

const kitEl = (id, html) => {
  let el = document.getElementById(id);
  if (!el) {
    el = document.createElement('div');
    el.id = id;
    el.innerHTML = html;
    (document.getElementById('stage') || document.body).appendChild(el);
  }
  return el;
};

/* A pointer that glides and clicks.
     moves:  [[t, x, y], ...]  start gliding toward (x, y) at time t
     clicks: [t, ...]          press and ripple at time t
   Time a click for after the glide lands: a move settles about one camera
   period (0.6s) after it starts. Use pressed(t, click) to make the clicked UI
   respond on the same frame the ripple starts. */
function cursor(t, { moves, clicks = [], size = 36, visible = 1 }) {
  const el = kitEl('kit-cursor',
    '<div class="kit-ripple"></div>' +
    '<svg viewBox="0 0 24 24"><path d="M3 2l7.5 19 2.6-7.9L21 10.5z" fill="#fff" stroke="#111" stroke-width="1.5" stroke-linejoin="round"/></svg>');
  const s = size * layout().u;
  const x = track(t, moves.map(([tt, xx]) => [tt, xx]), SPRING.camera);
  const y = track(t, moves.map(([tt, , yy]) => [tt, yy]), SPRING.camera);
  let press = 0, ring = 0, ringAlpha = 0;
  for (const c of clicks) {
    const d = t - c;
    if (d >= 0 && d < 0.18) press = Math.max(press, Math.sin((d / 0.18) * Math.PI));
    if (d >= 0 && d < 0.6) { ring = span(d, 0, 0.6); ringAlpha = 1 - ring; }
  }
  Object.assign(el.style, {
    position: 'absolute', left: 0, top: 0, width: s + 'px', height: s + 'px', zIndex: 1000,
    transform: `translate(${x}px, ${y}px) scale(${1 - 0.18 * press})`, transformOrigin: '0 0',
    opacity: visible, pointerEvents: 'none',
  });
  Object.assign(el.firstChild.style, {
    position: 'absolute', left: -s + 'px', top: -s + 'px', width: 2 * s + 'px', height: 2 * s + 'px',
    borderRadius: '50%', border: `${0.08 * s}px solid rgba(255,255,255,.9)`, boxSizing: 'border-box',
    transform: `scale(${0.2 + 0.8 * ring})`, opacity: ringAlpha,
  });
  return { x, y };
}
/* 0 until the click, then springs to 1: drive the clicked UI's response with it. */
const pressed = (t, click, preset = SPRING.ui) => spring(t - click, preset);

/* A terminal that types commands and prints their output, deterministically.
     lines: [{ cmd: 'npm test' }, { out: 'PASS  12 tests' }, { pause: 0.5 }, ...]
   Commands type at cps characters a second from `at`; each output line lands
   one `gap` after the previous line. Paste in real output, never invented
   output. Returns the time the last line lands, so the next beat can chain. */
function terminal(el, t, { at = 0, lines, cps = 28, gap = 0.08, prompt = '$ ' }) {
  const esc = (s) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' }[c]));
  let clock = at, html = '', typing = false;
  for (const line of lines) {
    if (line.pause) { clock += line.pause; continue; }
    if (line.cmd != null) {
      const shown = Math.floor((t - clock) * cps);
      if (shown >= 0) {
        html += `<span class="kit-prompt">${esc(prompt)}</span>${esc(line.cmd.slice(0, shown))}`;
        typing = shown < line.cmd.length;
        // No break: every line still advances the clock, so the return value is the same for any t.
        if (!typing) html += '\n';
      }
      clock += line.cmd.length / cps + 0.35; // a beat before the command "runs"
    } else if (line.out != null) {
      if (t >= clock) html += `<span class="kit-out">${esc(line.out)}</span>\n`;
      clock += gap;
    }
  }
  const caret = typing || Math.floor(t * 2) % 2 === 0 ? '<span class="kit-caret">▌</span>' : '';
  el.innerHTML = html + caret;
  return clock;
}
