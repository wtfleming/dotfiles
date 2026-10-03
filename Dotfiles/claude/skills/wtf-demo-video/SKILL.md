---
name: wtf-demo-video
description: >-
  Make a short motion-graphics demo video of a project — a launch clip, a
  product reel, a feature demo — written as code and rendered to MP4. Every
  animation is one deterministic window.seek(t) that paints the exact frame for
  time t; Playwright screenshots each frame and ffmpeg encodes them. Uses the
  project's real UI screenshots, a beat sheet of moments rather than
  adjectives, and a render-then-critique loop on the frames. Use when the user
  asks for a demo video, launch video, promo clip, product reel, animated
  walkthrough or "a video of this project": "make a video demoing this", "build
  a 15-second launch video", "/wtf-demo-video". NOT for screen-recording a live
  session, editing existing footage, an interactive explainer someone clicks
  through (wtf-simulate), or a GIF of a UI bug.
argument-hint: '[what to demo, length, format — e.g. "15s launch video, 16:9 and 9:16"]'
---

# Demo video

Arguments: $ARGUMENTS

A one-line prompt gets you a generic clip: centred text, a gradient, everything
fading in. What makes these good is the harness around the model — a
deterministic renderer, a real reference, a beat sheet of moments, and you
looking hard at your own frames. This skill is that harness.

## The engine contract

Everything follows from this. Break it and frames stutter, drift or differ
between renders.

- The film is one `index.html`. It sets `window.DURATION` (seconds) and defines
  `window.seek(t)`, which paints the exact frame for time `t` from nothing but
  `t`. It may return a promise. If it needs something loaded first, such as a
  web font no element has used yet, it sets `window.READY` to a promise, and
  the renderer waits for it.
- No timers, `setInterval`, `requestAnimationFrame`-driven state, CSS
  transitions or CSS animations. No unseeded `Math.random()` and no `Date`.
  Anything that accumulates from frame to frame breaks seeking to an arbitrary
  `t`.
- No `will-change`. A cached compositor layer repaints text slightly
  differently depending on which frame came before.
- Easing is a closed-form spring of time (`spring` and `track` in `kit.js`),
  so frame 600 never needs frames 0–599. Springs land exactly on their target,
  so a film can loop.
- Sizes derive from the frame (`layout()`), never fixed pixels, so one timeline
  renders at 16:9, 9:16 and 1:1. Reframe per format; don't crop.
- Canvas, DOM or SVG are all fine. Default to DOM with transforms, since text
  stays crisp and screenshots drop in as `<img>`. Don't reach for Remotion or
  another framework unless the user asks for one.

The skill's files, all under `~/.claude/skills/wtf-demo-video/`:

| File | What it does |
|---|---|
| `assets/index.html` | The starter film. Replace its example timeline. |
| `assets/kit.js` | `spring`, `track`, `layout`, `place`, plus `cursor()`, a pointer that glides and clicks, and `terminal()`, which types commands and prints their output. Read its header comments before using them. |
| `scripts/check-deps.sh` | Lists every missing dependency with its install command. |
| `scripts/capture.mjs` | Screenshots states of the running app from a shot list. |
| `scripts/render.mjs` | Renders stills, the video, or a loop check. |
| `scripts/beat-track.mjs` | Synthesizes a backing track and its beat grid. |
| `scripts/lib.mjs` | Shared setup for the scripts: loading playwright, reading flags. |

## 1. Check dependencies, then set up the video directory

Before anything else, run `~/.claude/skills/wtf-demo-video/scripts/check-deps.sh`.
It lists every missing tool with its install command.

- **`node` or `ffmpeg` missing:** tell the user exactly what is missing and the
  `brew install` line for it, then stop. Don't install system packages
  yourself, and don't hand-roll a substitute encoder.
- **Only playwright missing:** this is expected on a first run. It is a
  per-directory npm package, and the setup below installs it.

Work in `~/demo-videos/<project>/` unless the user names somewhere else.
**Don't put it inside the project:** a dev server under `cargo watch`,
`nodemon`, Vite and the like restarts when files appear there, and an
`npm install` adds thousands of them.

```sh
D=~/demo-videos/<project> && mkdir -p "$D/assets" "$D/refs" && cd "$D"
cp ~/.claude/skills/wtf-demo-video/assets/{index.html,kit.js} .
npm init -y >/dev/null && npm i -D playwright && npx playwright install chromium
~/.claude/skills/wtf-demo-video/scripts/check-deps.sh   # should now pass
```

Open `index.html` with `?t=3` in a browser to hold a frame. Without `?t` it
loops in real time.

## 2. Gather real material

The difference between a demo and a stock template is the actual product.

- **Pitch.** Read the README and the code until you can say in one sentence
  what the project does and who it is for. That sentence is the logline.
  Take the palette and fonts from the project's own stylesheet, so the film
  looks like the product.
- **Screens.** Capture the real UI with `capture.mjs`. It takes a shot list of
  pages, the steps to reach each state (click, fill, type, press, hover,
  waitFor, wait) and an optional crop. `"fullPage": true` captures the whole
  scroll height, for a camera pan. The script's header documents the format.

  ```sh
  node ~/.claude/skills/wtf-demo-video/scripts/capture.mjs shots.json --out assets   # 1440x900 at 2x
  ```

  - Survey the site first with a throwaway list at `--dpr 1`, and look at
    every shot. Routes that exist can still render empty or as a 404 in a dev
    database.
  - Use `type` rather than `fill` for search boxes, since they usually react to
    keyup.
  - Don't create accounts or write data in the user's database just to reach a
    logged-in state. Ask first, or leave those features out.
  - For a CLI, paste real command output into `terminal()` rather than
    inventing it.
  - Don't invent features, numbers or quotes.
- **Reference.** Ask the user for a reference: a frame from a video they like
  (`refs/frame.png`), a video, or a style to name. Say what to take from it,
  such as palette, type, pacing or grain, and what to ignore, which is usually
  the subject. Naming a style beats describing one. With a video reference,
  pull a frame per second with `ffmpeg -i ref.mp4 -vf fps=1 refs/%03d.png` and
  describe its pacing before writing code. With no reference, the project's own
  look is the reference. Don't fall back to the default gradient. The
  `frontend-design` skill helps here.

## 3. Write the beat sheet before any code

Describe the video as moments, not adjectives: what is on screen at each
timestamp and what changes between them. Use the user's beat sheet if they
gave one. Otherwise draft one and get it approved, since it is the user's
call. The shape that works for a 15-second launch:

```
0s   logo
2s   tagline
4s   the main screen
8s   one feature in action — a cursor makes real clicks, the UI responds
12s  pricing or CTA
14s  logo again, settled by 15s to match frame 0, so it loops
```

Rules for the sheet:

- A visual payoff every 3–5 seconds.
- One element morphs between states (button → loader → panel → chart) instead
  of hard cuts.
- On-screen text short enough to read twice in the time it is up.
- The first 3 seconds carry the video. Spend the most polish there.
- At 120 BPM a 4/4 bar is 2s, so the timestamps above already fall on
  downbeats. Keep scene changes there if there will be music.

Techniques that worked:

- **Interaction inside a screenshot.** Draw the typed text over the real input
  box, then reveal the after-state screenshot through a `clip-path` that
  springs open. A dropdown slides open this way.
- **Navigation.** Slide the next page up over the current one, opaque. A
  crossfade of two busy screenshots reads as mush.
- **Colour morphs.** Blend in OKLCH, but hold the hue when one end is
  near-grey. Otherwise the blend swings through unrelated hues: amber to ink
  passes through teal.

## 4. Build, render, critique, repeat

Build the timeline in `index.html`, replacing the template's example film. Then
loop:

```sh
R=~/.claude/skills/wtf-demo-video/scripts/render.mjs
node $R --stills stills/   # one PNG per second
rows=$(( ($(ls stills/t*.png | wc -l) + 3) / 4 ))
ffmpeg -loglevel error -y -i stills/t%03d.png \
  -vf "scale=480:-1,tile=4x$rows:padding=6:color=0x222222" -frames:v 1 contact.png
```

Read `contact.png`, then the individual stills that look wrong. Whole seconds
miss transitions, so also check stills at the moments between beats, where
things morph, cross or hand over. Score each second out of 10, **name the
three worst problems**, fix them, and re-render. Common problems:

- collisions and clipping
- text too small for the format
- dead air
- two things competing for the eye
- an element popping in with no motion
- muddy mid-transition frames
- the default "centred text, everything fades in" look creeping back

Repeat until every still scores 8 or more and the first three seconds look
finished. If the film should loop, check that it does:

```sh
node $R --check-loop   # exit 1, and the region that differs, if t=0 and t=DURATION don't match
```

Then render the video. By default it spreads frames across 4 browser pages,
and `--workers` changes that.

```sh
node $R --out final.mp4                        # 1920x1080, 60fps
node $R --out final-9x16.mp4 --size 1080x1920
```

Check the encode with `ffprobe -v error -show_entries format=duration final.mp4`.
Review the encoded file itself, not just the page:
`ffmpeg -i final.mp4 -vf fps=1 stills/v%02d.png` for the whole film, or a burst
of frames such as `ffmpeg -ss 3.5 -i final.mp4 -frames:v 6 -vf fps=12 stills/m%02d.png`
for motion.

## 5. Optional extras, only when asked

- **Springs everywhere.** Replace every remaining linear or eased tween with
  `track`/`spring`. Give UI a tiny overshoot (`SPRING.ui`) and type none
  (`SPRING.type`).
- **Soundtrack.** Either measure a supplied track's BPM and put every scene
  change on a downbeat, or synthesize one:

  ```sh
  node ~/.claude/skills/wtf-demo-video/scripts/beat-track.mjs --duration 15 --bpm 120 --key A
  ```

  That writes `track.wav` and `beats.json`, the downbeat times to cut on. Mux
  the track in with `render.mjs --audio track.wav`. If the user has a TTS or
  music API key, it goes in `.env`; read it from the environment and never
  write it into a file.
- **More formats.** Render 16:9, 9:16 and 1:1 from the same `seek(t)` by
  varying `--size`, and branch on `layout().portrait` where the composition
  needs reframing. Desktop screenshots letterbox badly in 9:16. Zoom the window
  in on the region that matters rather than shrinking the whole page to fit.

## 6. Deliver

Hand over:

- `final.mp4`, plus any other formats
- a poster frame: `ffmpeg -ss 1 -i final.mp4 -frames:v 1 poster.png`
- the path to the video directory, so the user can re-render after a one-line
  edit

Send the MP4 with `SendUserFile` when it is available. Report what the critique
loop fixed, and anything you knowingly left rough.
