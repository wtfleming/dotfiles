# Craft

Numbers for the beat sheet and the critique. They are defaults: the user's
reference, or a beat that breaks one on purpose, wins for that beat.

## Story

- **Frame 0 is the thumbnail.** Open on the product already doing its thing,
  or the moment that creates the need. Never black, never a logo alone, and
  never "Introducing" or "Meet" as the first line.
- **One viewer, one real task.** One real input (a query, a file, a command)
  travels from the hook to the payoff. A list of features is not a story.
- **A launch shape**, scaled to the length: hook ~8%, the old way failing
  ~8%, the product named ~8%, proof in the real UI ~40% as 3–6 verbs, the
  payoff ~20% and played at normal speed, the one-line claim ~8%, the end
  card ~8%.
- **Show the product, then the part, then the product.** Establish the real
  workspace; bring the one control or result forward at 1.6–2× its real size,
  with its real labels; then carry it back into place. A camera drifting over
  a frozen screenshot is not the product acting.
- **The mark lands once**, in the last 5–10%, centred, 15–30% of the frame
  width, carried or wiped in rather than faded. The call to action (a real
  button, the command to type) holds 1.5–3 s.
- **Looping is a choice, not a default.** A launch ends on its mark; a loop
  returns to frame 0's shot instead.

## Text

- At least 28 px at 1080 (2.6% of the frame's short side), after any zoom.
  Too many words: cut words, never shrink type.
- At most 6 words a line, one line on screen at a time, no trailing periods.
- A line holds 0.5 s plus 0.3 s a word; the film's last line at least 2 s.
- A landed title is dead still. The motion moves to the next element.
- Titles 4–9% of the frame height. One highlighted word in the accent colour.
- No "unlock", "seamless", "supercharge", "game-changer", "effortless", or
  "in seconds" unless measured.

## Motion

- **Spring by role**: arrivals land on a long tail (`SPRING.type` for text,
  `SPRING.ui` for cards); camera moves ease both ways (`SPRING.camera`).
  Position and size may overshoot; opacity and colour never do. Damping 1.0
  never bounces, 0.7–0.85 reads premium, 0.4–0.6 playful.
- **Exits** are 40–50% shorter than entrances, and one smooth motion: never
  slide, stop, then leave.
- **Overlap**: the next element starts when the previous one is 30–50% done.
  Stagger letters a frame apart, words 60–120 ms, list items 40–80 ms.
- **One hero move per shot.** Secondary moves travel 50–70% as far and start
  100–200 ms after it; everything else holds still. The hero fills at least
  40% of the frame's height or width.
- **Rhythm**: something new every 2–4 s; a demo can hold one framing for
  7–10 s while the UI acts. Hold 0.3–0.75 s before the payoff.
- **Cut at the fastest frame** of a move, with the same direction and speed
  on both sides. A click into a new screen cuts on the click; the new screen
  arrives slightly large and settles.

## The cursor

`cursor()` in `kit.js` follows these; the timing is yours.

- Big: 4–7% of the frame. An actual-size pointer disappears at video scale.
- It enters from off-frame on one continuous path and leaves the same way.
  Never fade it in or out where it sits.
- Every click causes something on the same frame (`pressed(t, click)`).
- During a beat it doesn't own (typing, a long hold) it drifts aside. It
  never sits on top of the action and never idles.

## Transitions with a cause

Use 3–5 kinds in a film, each caused by something on screen. Hard cuts belong
inside a scene; screens never crossfade into each other.

| Transition | How |
|---|---|
| Zoom through a button | the camera tracks to it (0.6 s), then it scales to ~250% of the frame in 0.35 s, accelerating; its fill becomes the next frame |
| Object becomes the world | a card grows past the frame in 0.65 s, its radius shrinking; the new content appears while it still grows |
| Morph | one container widens into the next (icon → input box) |
| Iris from the click | `clip-path: circle()` opens from the clicked point in 0.15 s |
| Colour flood | the hero stays while the ground changes colour in ~0.4 s |
| Dip cut | the outgoing frame darkens ~40% for 3 frames before a hard cut |

## Defaults that look generated

Banned unless the brand's own material or the user's reference uses them:

- a dark void with small things floating in its middle
- glow, bloom, lens flares, particles, purple or blue gradients, glass cards
- everything centred; everything fading in at once, or fading and sliding up
- breathing, floating or pulsing loops standing in for motion on titles and UI
- text that moves while it is read; titles drifting after they land
- three cards in a row, KPI grids, a title over a dimmed screenshot
- click rings, bounce on UI, the logo docked in a corner
- every scene entering the same way

When a render shows one, name it, take the replacement from the product or
the reference, and add it to the beat sheet as banned for this film.

## Reviewing

- Describe each beat frame in one sentence as a stranger would. Anything you
  would have to explain is a problem.
- Cover the words: do the pictures alone still tell it?
- Look at the frames either side of every cut (`--at` with the cut time
  ±0.2 s): a jump there is a problem.
- Write each problem as `t=4.2s · where · problem → exact change`. P0 breaks
  the read (unreadable, wrong, a banned default in the hero shot); P1 weakens
  it (dead stretch, soft payoff, jumping seam); P2 is polish. Fix the P0s,
  text first, then timing, then detail.
- A film that feels busy: remove a third of the motion and keep the moves
  that lead the eye.

## 9:16

Recompose, never squeeze. On 1080×1920 the platform draws over the frame:
nothing in the top 168 px; every word inside the band from 168 to 1330 px;
below 1330 px only texture; no text under the right-hand action rail. Titles
about 1.35× their landscape size, at most 5 words a line, shots of 1–2 s.

---

Adapted from the motion, story, spec, review and vertical references and the
oversized-cursor skill in HyperFrames by HeyGen (Apache-2.0, see
`LICENSE-hyperframes`).
