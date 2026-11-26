def current_user(request):
    """The signed-in user's id, or None. Set by the login handler."""
    return request.session.get("user_id")


def require_admin(handler):
    def wrapped(request, *args):
        if not request.session.get("is_admin"):
            return 403, "forbidden"
        return handler(request, *args)
    return wrapped
