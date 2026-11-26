from app import db
from app.session import current_user

SORT_COLUMNS = {"title": "title", "newest": "created_at"}


def list_notes(request):
    user = current_user(request)
    if user is None:
        return 401, "sign in"
    column = SORT_COLUMNS.get(request.args.get("sort"), "created_at")
    sql = "SELECT id, title FROM notes WHERE owner_id = ? ORDER BY " + column
    return 200, db.query(sql, (user,))
