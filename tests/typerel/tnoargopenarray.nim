discard """
action: compile
"""

import std/db_sqlite

var db: DbConn
exec(db, sql"create table blabla()")
