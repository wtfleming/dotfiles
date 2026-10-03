#!/usr/bin/env bash
# Report every missing dependency at once, with its install command.
# Exits 1 if anything is missing.
#   check-deps.sh [video-dir]   # video-dir defaults to the cwd

dir=${1:-.}
missing=0

need() { # binary, install hint
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "missing: $1 — install with: $2"
        missing=1
    fi
}

need node    "brew install node"
need ffmpeg  "brew install ffmpeg"
need ffprobe "brew install ffmpeg"

if command -v node >/dev/null 2>&1; then
    if ! (cd "$dir" 2>/dev/null && node -e "require.resolve('playwright')" >/dev/null 2>&1); then
        echo "missing: playwright in $dir — install with: (cd $dir && npm i -D playwright && npx playwright install chromium)"
        missing=1
    elif ! (cd "$dir" && node -e "process.exit(require('fs').existsSync(require('playwright').chromium.executablePath()) ? 0 : 1)") >/dev/null 2>&1; then
        echo "missing: playwright's chromium — install with: (cd $dir && npx playwright install chromium)"
        missing=1
    fi
fi

[ "$missing" -eq 0 ] && echo "all dependencies present"
exit "$missing"
