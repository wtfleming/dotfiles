import html

from app import db
from app.session import current_user


def list_notes(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    return 200, db.query("SELECT id, title FROM notes WHERE owner_id = ?", (user,))


def search_page(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    term = request.args.get("q", "")
    rows = db.query("SELECT id, title FROM notes WHERE owner_id = ? AND title = ?", (user, term))
    items = "".join(f"<li>{html.escape(title)}</li>" for _, title in rows)
    return 200, f"<h1>Results for {html.escape(term)}</h1><ul>{items}</ul>"
