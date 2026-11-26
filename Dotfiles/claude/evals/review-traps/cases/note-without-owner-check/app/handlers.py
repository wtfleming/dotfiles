from app import db
from app.session import current_user


def list_notes(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    return 200, db.query("SELECT id, title FROM notes WHERE owner_id = ?", (user,))


def show_note(request, note_id):
    if current_user(request) is None:
        return 401, "sign in"
    rows = db.query("SELECT id, title, body FROM notes WHERE id = ?", (note_id,))
    if not rows:
        return 404, "no such note"
    return 200, rows[0]
