import sqlite3

_conn = sqlite3.connect(":memory:")


def query(sql, params=()):
    return _conn.execute(sql, params).fetchall()
