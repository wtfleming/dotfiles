import subprocess

from app import db
from app.session import current_user


def list_notes(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    return 200, db.query("SELECT id, title FROM notes WHERE owner_id = ?", (user,))


def recent_changes(request):
    if current_user(request) is None:
        return 401, "sign in"
    try:
        count = int(request.args.get("n", "10"))
    except ValueError:
        return 400, "n must be a number"
    count = max(1, min(count, 100))
    out = subprocess.run(
        ["git", "log", "--oneline", f"-{count}"],
        capture_output=True, text=True, timeout=10, check=False,
    )
    return 200, out.stdout
