/* render.mjs — render a seek(t) page to video, frame by frame.
 *
 *   node render.mjs [index.html] [--out final.mp4] [--fps 60] [--size 1920x1080]
 *                   [--workers 4] [--audio track.wav] [--stills stills/] [--check-loop]
 *
 * The page must define window.DURATION (seconds) and window.seek(t), which paints
 * the exact frame for time t and may return a promise. It may also set
 * window.READY to a promise the renderer awaits first, e.g. for web fonts that
 * no element has used yet. Frames are seek()ed and
 * screenshotted across --workers browser pages at once (seek is pure, so any page
 * can paint any frame), then piped in order straight into ffmpeg.
 *
 * --stills DIR    skip the video; write one PNG per whole second for review.
 * --check-loop    skip the video; compare the frame at 0 with the frame at DURATION,
 *                 write loop-first.png / loop-last.png, exit 1 if they differ.
 *
 * Requires ffmpeg on PATH, and playwright installed in the working directory:
 *   npm i -D playwright && npx playwright install chromium
 * Exits 1 on any page error, so a broken frame never ships silently.
 */
import { pathToFileURL } from 'node:url';
import { spawn, spawnSync } from 'node:child_process';
import { mkdirSync, existsSync, writeFileSync, rmSync, renameSync } from 'node:fs';
import { availableParallelism } from 'node:os';
import path from 'node:path';
import { loadChromium, flag as argFlag } from './lib.mjs';

const chromium = loadChromium();
const args = process.argv.slice(2);
const VALUED = ['out', 'fps', 'size', 'workers', 'audio', 'stills'];
const flag = (name, dflt) => argFlag(args, name, dflt);
const positional = args.filter((a, i) => !a.startsWith('--') && !VALUED.includes((args[i - 1] || '').slice(2)));

const filmPath = path.resolve(positional[0] || 'index.html');
const out = flag('out', 'final.mp4');
// ffmpeg writes here and the result replaces `out` only on success, so a failed render never costs the last good one.
const partial = path.join(path.dirname(out), path.basename(out, path.extname(out)) + '.partial' + path.extname(out));
const fps = Number(flag('fps', 60));
const [width, height] = flag('size', '1920x1080').split('x').map(Number);
const workers = Number(flag('workers', Math.min(4, availableParallelism())));
const audio = flag('audio', null);
const stillsDir = flag('stills', null);
const checkLoop = args.includes('--check-loop');

if (!existsSync(filmPath)) {
  console.error(`no such page: ${filmPath}`);
  process.exit(2);
}
if (!(fps > 0 && width > 0 && height > 0 && workers >= 1)) {
  console.error('--fps and --workers must be positive, and --size WIDTHxHEIGHT');
  process.exit(2);
}
if (!stillsDir && spawnSync('ffmpeg', ['-version']).error) {
  console.error('ffmpeg not found. Install it with:  brew install ffmpeg');
  process.exit(2);
}

const browser = await chromium.launch();
let ff = null, done = null;
try {
  // Each page keeps its own errors, so a failure is pinned to the frame that page was painting.
  const openPage = async () => {
    const page = await browser.newPage({ viewport: { width, height }, deviceScaleFactor: 1 });
    page.errors = [];
    page.on('pageerror', (e) => page.errors.push(e.message));
    page.on('console', (m) => {
      if (m.type() !== 'error') return;
      const { url } = m.location();
      page.errors.push(url ? `${m.text()} (${url})` : m.text());
    });
    await page.goto(pathToFileURL(filmPath).href, { waitUntil: 'load' });
    // Fonts and images that land mid-render swap in on a random frame.
    // window.READY is the film's own promise for anything else it must load first.
    await page.evaluate(async () => {
      await window.READY;
      await document.fonts.ready;
      await Promise.all([...document.images].map((img) => img.decode().catch(() => {})));
    });
    if (page.errors.length) throw new Error(`page error while loading ${filmPath}: ${page.errors[0]}`);
    return page;
  };
  const pages = [await openPage()];
  const duration = await pages[0].evaluate(() => window.DURATION);
  if (typeof duration !== 'number' || duration <= 0) throw new Error('window.DURATION is not a positive number');
  if (!(await pages[0].evaluate(() => typeof window.seek === 'function'))) throw new Error('window.seek is not defined');

  const shoot = async (page, t) => {
    const at = `t=${t.toFixed(3)}s`;
    try {
      await page.evaluate((t) => window.seek(t), t);
    } catch (e) {
      throw new Error(`seek threw at ${at}: ${e.message.split('\n')[0]}`);
    }
    if (page.errors.length) throw new Error(`page error at ${at}: ${page.errors[0]}`);
    return page.screenshot({ type: 'png' });
  };

  if (checkLoop) {
    const first = await shoot(pages[0], 0), last = await shoot(pages[0], duration);
    writeFileSync('loop-first.png', first);
    writeFileSync('loop-last.png', last);
    // A whole-frame average hides a small leftover element, so count changed pixels.
    // A channel off by more than 8/255 is a real change; compositing noise stays under 6.
    const cmp = await browser.newPage();
    const r = await cmp.evaluate(async ([a, b]) => {
      const pixels = async (b64) => {
        const img = new Image();
        img.src = 'data:image/png;base64,' + b64;
        await img.decode();
        const c = new OffscreenCanvas(img.width, img.height).getContext('2d');
        c.drawImage(img, 0, 0);
        return c.getImageData(0, 0, img.width, img.height);
      };
      const A = await pixels(a), B = await pixels(b);
      let n = 0, x0 = Infinity, y0 = Infinity, x1 = -1, y1 = -1;
      for (let i = 0; i < A.data.length; i += 4) {
        const d = Math.max(Math.abs(A.data[i] - B.data[i]), Math.abs(A.data[i + 1] - B.data[i + 1]), Math.abs(A.data[i + 2] - B.data[i + 2]));
        if (d > 8) {
          const p = i / 4, x = p % A.width, y = Math.floor(p / A.width);
          n++; x0 = Math.min(x0, x); y0 = Math.min(y0, y); x1 = Math.max(x1, x); y1 = Math.max(y1, y);
        }
      }
      return { n, total: A.width * A.height, box: n ? `${x1 - x0 + 1}x${y1 - y0 + 1}+${x0}+${y0}` : '' };
    }, [first.toString('base64'), last.toString('base64')]);
    // Tolerate a few stray antialiased edge pixels, nothing a viewer would see as a jump.
    const ok = r.n <= r.total * 1e-4;
    console.log(ok
      ? `loop seamless: t=0 and t=${duration} match (${r.n} stray pixels)`
      : `loop BREAKS: ${r.n} pixels differ between t=0 and t=${duration}, in region ${r.box} (see loop-first.png, loop-last.png)`);
    if (!ok) process.exitCode = 1;
  } else if (stillsDir) {
    mkdirSync(stillsDir, { recursive: true });
    for (let s = 0; s <= Math.floor(duration); s++) {
      // The last whole second can equal DURATION, one frame past the end.
      const t = Math.min(s, duration - 1 / fps);
      writeFileSync(path.join(stillsDir, `t${String(s).padStart(3, '0')}.png`), await shoot(pages[0], t));
    }
    console.log(`wrote ${Math.floor(duration) + 1} stills to ${stillsDir}`);
  } else {
    while (pages.length < workers) pages.push(await openPage());
    const frames = Math.round(duration * fps);
    ff = spawn('ffmpeg', [
      '-y', '-loglevel', 'error',
      '-f', 'image2pipe', '-framerate', String(fps), '-c:v', 'png', '-i', '-',
      ...(audio ? ['-i', audio, '-c:a', 'aac', '-b:a', '192k', '-shortest'] : []),
      '-c:v', 'libx264', '-pix_fmt', 'yuv420p', '-crf', '16', '-preset', 'slow',
      '-movflags', '+faststart', partial,
    ], { stdio: ['pipe', 'inherit', 'inherit'] });
    done = new Promise((res, rej) => {
      ff.on('error', rej);
      ff.on('close', (code) => (code === 0 ? res() : rej(new Error(`ffmpeg exited ${code}`))));
    });
    // ffmpeg can exit mid-render (a bad --audio path); report that, not an unhandled rejection or EPIPE.
    done.catch(() => {});
    ff.stdin.on('error', () => {});
    const started = Date.now();
    // One batch is one frame per page; writing each batch in order keeps the stream ordered.
    for (let i = 0; i < frames; i += pages.length) {
      if (ff.exitCode !== null) await done;
      const batch = await Promise.all(pages.map((p, j) => (i + j < frames ? shoot(p, (i + j) / fps) : null)));
      for (const buf of batch) {
        if (buf && !ff.stdin.write(buf)) await Promise.race([new Promise((r) => ff.stdin.once('drain', r)), done]);
      }
      if (Math.floor(i / fps) !== Math.floor((i + pages.length) / fps)) process.stdout.write(`\r${Math.min(i + pages.length, frames)}/${frames} frames`);
    }
    ff.stdin.end();
    await done;
    ff = null;
    renameSync(partial, out);
    const secs = ((Date.now() - started) / 1000).toFixed(1);
    console.log(`\rwrote ${out}: ${frames} frames, ${duration}s at ${fps}fps, ${width}x${height}, ${pages.length} workers, ${secs}s`);
  }
} catch (e) {
  console.error(e.message);
  process.exitCode = 1;
  // A live ffmpeg would wait on stdin forever and keep the process from exiting.
  if (ff) {
    ff.stdin.destroy();
    ff.kill('SIGKILL');
    rmSync(partial, { force: true });
  }
} finally {
  await browser.close();
}
