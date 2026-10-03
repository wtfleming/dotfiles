/* capture.mjs — screenshot real states of a running app, for use as film assets.
 *
 *   node capture.mjs shots.json [--out assets] [--size 1440x900] [--dpr 2] [--dark]
 *
 * shots.json:
 *   {
 *     "base": "http://localhost:3000",
 *     "shots": [
 *       { "name": "home", "path": "/" },
 *       { "name": "search-results", "path": "/",
 *         "steps": [ { "fill": ["input[name=q]", "spring physics"] },
 *                    { "press": "Enter" },
 *                    { "waitFor": ".results" } ],
 *         "selector": "main" }
 *     ]
 *   }
 *
 * Steps run in order: click (selector), fill ([selector, text]), type ([selector,
 * text] as real keystrokes, for inputs that react to keyup), press (key), hover
 * (selector), waitFor (selector), wait (ms). "selector" crops the shot to one
 * element; "fullPage": true captures the whole scroll height, for a camera pan;
 * otherwise the viewport is captured. All shots share one browser
 * context, so a login step in an early shot carries over to later ones.
 *
 * A failing shot is reported and skipped; the rest still run. Exits 1 if any failed.
 * Requires playwright installed in the working directory (see render.mjs).
 */
import { mkdirSync, readFileSync } from 'node:fs';
import path from 'node:path';
import { loadChromium, flag as argFlag } from './lib.mjs';

const chromium = loadChromium();
const args = process.argv.slice(2);
const flag = (name, dflt) => argFlag(args, name, dflt);
const list = args[0];
if (!list || list.startsWith('--')) {
  console.error('usage: node capture.mjs shots.json [--out assets] [--size 1440x900] [--dpr 2] [--dark]');
  process.exit(2);
}
const { base = '', shots } = JSON.parse(readFileSync(list, 'utf8'));
// Each name is an output file, so a missing or repeated one would overwrite another shot.
const names = shots.map((s) => s.name);
const bad = names.findIndex((n, i) => !n || names.indexOf(n) !== i);
if (bad >= 0) {
  console.error(names[bad] ? `shot name "${names[bad]}" is used twice` : 'every shot needs a "name"');
  process.exit(2);
}
const outDir = flag('out', 'assets');
const [width, height] = flag('size', '1440x900').split('x').map(Number);
const dpr = Number(flag('dpr', 2));
mkdirSync(outDir, { recursive: true });

const browser = await chromium.launch();
const context = await browser.newContext({
  viewport: { width, height }, deviceScaleFactor: dpr,
  colorScheme: args.includes('--dark') ? 'dark' : 'light',
});
context.setDefaultTimeout(10000); // a mistyped selector fails in 10s, not 30
let failed = 0;
for (const shot of shots) {
  const page = await context.newPage();
  try {
    await page.goto(new URL(shot.path || '/', base || undefined).href, { waitUntil: 'load' });
    for (const [i, step] of (shot.steps || []).entries()) {
      try {
        if (step.click) await page.click(step.click);
        else if (step.fill) await page.fill(step.fill[0], step.fill[1]);
        else if (step.type) await page.locator(step.type[0]).pressSequentially(step.type[1], { delay: 60 });
        else if (step.press) await page.keyboard.press(step.press);
        else if (step.hover) await page.hover(step.hover);
        else if (step.waitFor) await page.waitForSelector(step.waitFor);
        else if (step.wait) await page.waitForTimeout(step.wait);
        else throw new Error('unknown step');
      } catch (e) {
        // Playwright's first line rarely names the selector, so name the step.
        throw new Error(`step ${i + 1} ${JSON.stringify(step)}: ${e.message.split('\n')[0]}`);
      }
    }
    // A blinking caret or a half-finished transition would otherwise be baked into the asset.
    await page.addStyleTag({ content: '*,*::before,*::after{transition:none!important;animation:none!important;caret-color:transparent!important}' });
    const file = path.join(outDir, `${shot.name}.png`);
    if (shot.selector) await page.locator(shot.selector).first().screenshot({ path: file, animations: 'disabled' });
    else await page.screenshot({ path: file, animations: 'disabled', fullPage: !!shot.fullPage });
    console.log(`ok    ${file}`);
  } catch (e) {
    failed++;
    console.error(`FAIL  ${shot.name}: ${e.message.split('\n')[0]}`);
  } finally {
    await page.close();
  }
}
await browser.close();
process.exitCode = failed ? 1 : 0;
