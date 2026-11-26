from app import db
from app.session import current_user


def list_notes(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    return 200, db.query("SELECT id, title FROM notes WHERE owner_id = ?", (user,))


def delete_any_note(request, note_id):
    db.query("DELETE FROM notes WHERE id = ?", (note_id,))
    return 204, ""
