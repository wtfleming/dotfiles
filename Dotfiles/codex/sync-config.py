# /// script
# requires-python = ">=3.11"
# dependencies = ["tomlkit==0.13.3"]
# ///
"""Merge the managed status line without replacing machine-local Codex settings."""

import os
from pathlib import Path
import sys
import tempfile

import tomlkit


def sync(source: Path, destination: Path) -> None:
    managed = tomlkit.parse(source.read_text())
    existing = destination.read_text() if destination.exists() else ""
    config = tomlkit.parse(existing)
    if "tui" not in config:
        config["tui"] = tomlkit.table()
    config["tui"]["status_line"] = managed["tui"]["status_line"]
    updated = tomlkit.dumps(config)
    if updated == existing:
        return

    # Validate before replacing the file, and leave the original intact on failure.
    tomlkit.parse(updated)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", dir=destination.parent, delete=False
        ) as output:
            temporary = Path(output.name)
            output.write(updated)
        if destination.exists():
            temporary.chmod(destination.stat().st_mode & 0o777)
        os.replace(temporary, destination)
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


if __name__ == "__main__":
    sync(Path(sys.argv[1]), Path(sys.argv[2]))
