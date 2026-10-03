/* beat-track.mjs — synthesize a simple loopable backing track, plus its beat grid.
 *
 *   node beat-track.mjs --duration 15 [--bpm 120] [--key A] [--out track.wav]
 *
 * Writes a 44.1kHz 16-bit stereo WAV (kick on every beat, hat on the off-beats,
 * bass and a pad following a minor i–VI–III–VII progression, one chord a bar),
 * and beats.json beside it:
 *   { "bpm": 120, "beat": 0.5, "beats": [0, 0.5, ...], "downbeats": [0, 2, ...] }
 * Put scene changes on downbeats (the first beat of each 4/4 bar). Seeded, so
 * the same arguments always produce the same file. Mux it in with
 * render.mjs --audio track.wav.
 */
import { writeFileSync } from 'node:fs';
import path from 'node:path';

const args = process.argv.slice(2);
const flag = (name, dflt) => {
  const i = args.indexOf('--' + name);
  return i >= 0 && args[i + 1] ? args[i + 1] : dflt;
};
const duration = Number(flag('duration', NaN));
const bpm = Number(flag('bpm', 120));
const key = flag('key', 'A');
const out = flag('out', 'track.wav');
const NOTES = { C: 0, 'C#': 1, D: 2, 'D#': 3, E: 4, F: 5, 'F#': 6, G: 7, 'G#': 8, A: 9, 'A#': 10, B: 11 };
if (!(duration > 0 && bpm > 0) || !(key in NOTES)) {
  console.error('usage: node beat-track.mjs --duration SECONDS [--bpm 120] [--key A|C#|...] [--out track.wav]');
  process.exit(2);
}

const RATE = 44100;
const n = Math.round(duration * RATE);
const L = new Float32Array(n), R = new Float32Array(n);
const beat = 60 / bpm, bar = 4 * beat;
const hz = (semi) => 440 * 2 ** ((semi - 9) / 12); // semitones above C4
const root = NOTES[key] - 12;                         // the key's root, an octave down
const PROGRESSION = [[0, 3, 7], [-4, 0, 3], [3, 7, 10], [-2, 2, 5]]; // i VI III VII, minor

let seed = 1;
const noise = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 31) - 1;

// Adds `fn(age)` into both channels from `start` for `len` seconds, panned.
const add = (start, len, fn, pan = 0) => {
  const a = Math.round(start * RATE), b = Math.min(n, Math.round((start + len) * RATE));
  for (let i = Math.max(0, a); i < b; i++) {
    const v = fn((i - a) / RATE);
    L[i] += v * (1 - pan) / 2;
    R[i] += v * (1 + pan) / 2;
  }
};

for (let b = 0; b * beat < duration; b++) {
  const t = b * beat;
  // Kick: a sine pitch-dropping 150Hz → 45Hz.
  add(t, 0.35, (a) => 0.9 * Math.exp(-a * 9) * Math.sin(2 * Math.PI * (45 * a + (105 / 30) * (1 - Math.exp(-30 * a)))));
  // Hat: high-passed noise (difference of successive samples), on the off-beat.
  let prev = 0;
  add(t + beat / 2, 0.06, (a) => { const x = noise(); const v = x - prev; prev = x; return 0.12 * Math.exp(-a * 70) * v; }, 0.3);
}

for (let k = 0; k * bar < duration; k++) {
  const chord = PROGRESSION[k % PROGRESSION.length];
  const start = k * bar;
  // Bass: root on beats 1 and 3, a soft square-ish tone.
  for (const off of [0, 2 * beat]) {
    const f = hz(root + chord[0] - 12);
    add(start + off, beat * 1.8, (a) => 0.28 * Math.min(1, a * 200) * Math.exp(-a * 2.2) *
      (Math.sin(2 * Math.PI * f * a) + Math.sin(6 * Math.PI * f * a) / 3));
  }
  // Pad: slightly detuned triad, swelling in and out over the bar.
  for (const [i, semi] of chord.entries()) {
    const f = hz(root + semi + 12);
    add(start, bar, (a) => 0.05 * Math.sin(Math.PI * a / bar) *
      (Math.sin(2 * Math.PI * f * a) + Math.sin(2 * Math.PI * f * 1.003 * a)), (i - 1) * 0.5);
  }
}

// Peak-normalize to -1 dBFS, then write PCM.
let peak = 1e-9;
for (let i = 0; i < n; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
const gain = 10 ** (-1 / 20) / peak;
const buf = Buffer.alloc(44 + n * 4);
buf.write('RIFF', 0); buf.writeUInt32LE(36 + n * 4, 4); buf.write('WAVEfmt ', 8);
buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20); buf.writeUInt16LE(2, 22);
buf.writeUInt32LE(RATE, 24); buf.writeUInt32LE(RATE * 4, 28); buf.writeUInt16LE(4, 32); buf.writeUInt16LE(16, 34);
buf.write('data', 36); buf.writeUInt32LE(n * 4, 40);
for (let i = 0; i < n; i++) {
  buf.writeInt16LE(Math.round(L[i] * gain * 32767), 44 + i * 4);
  buf.writeInt16LE(Math.round(R[i] * gain * 32767), 46 + i * 4);
}
writeFileSync(out, buf);

const grid = (step) => Array.from({ length: Math.ceil(duration / step) }, (_, i) => +(i * step).toFixed(4));
const gridFile = path.join(path.dirname(out), 'beats.json');
writeFileSync(gridFile, JSON.stringify({ bpm, beat, beats: grid(beat), downbeats: grid(bar) }, null, 1) + '\n');
console.log(`wrote ${out} (${duration}s, ${bpm} BPM, ${key} minor) and ${gridFile}`);
