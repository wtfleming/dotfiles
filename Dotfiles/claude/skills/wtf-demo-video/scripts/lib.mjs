/* lib.mjs — setup shared by this skill's scripts. */
import { createRequire } from 'node:module';
import { pathToFileURL } from 'node:url';

/* ESM resolves a bare import relative to the importing file, which lives in the
   skill directory with no node_modules — so resolve playwright from the cwd.
   Exits 2 with the install command if it is missing, or with the real reason
   if it is installed but fails to load. */
export function loadChromium() {
  try {
    return createRequire(pathToFileURL(process.cwd() + '/'))('playwright').chromium;
  } catch (e) {
    // A dependency of playwright going missing is also MODULE_NOT_FOUND.
    if (e.code !== 'MODULE_NOT_FOUND' || !e.message.includes("'playwright'")) {
      console.error(`playwright failed to load: ${e.message}`);
    } else {
      console.error('playwright not found. From the video directory, run:');
      console.error('  npm i -D playwright && npx playwright install chromium');
    }
    process.exit(2);
  }
}

/* --name value from argv, or dflt when absent. Exits 2 if --name is given with no value. */
export function flag(args, name, dflt) {
  const i = args.indexOf('--' + name);
  if (i < 0) return dflt;
  const v = args[i + 1];
  if (v == null || v.startsWith('--')) {
    console.error(`--${name} needs a value`);
    process.exit(2);
  }
  return v;
}

/* Exits 2 on a --flag not in known: an ignored --fps=30 would run at the default. */
export function rejectUnknown(args, known) {
  const unknown = args.find((a) => a.startsWith('--') && !known.includes(a.slice(2)));
  if (unknown) {
    console.error(`unknown flag ${unknown} (pass a value as --name value)`);
    process.exit(2);
  }
}
