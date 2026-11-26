from app import handlers
from app.session import require_admin

ROUTES = {
    ("GET", "/notes"): handlers.list_notes,
    ("GET", "/search"): handlers.search_page,
}

ADMIN_ROUTES = {}

# Every admin route is wrapped here, once, so a handler cannot forget the check.
for key, handler in ADMIN_ROUTES.items():
    ROUTES[key] = require_admin(handler)
