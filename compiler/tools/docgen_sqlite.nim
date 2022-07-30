## Writing documentation database in sqlite format, either for further code
## analysis (via scripts), or sourcetrail integraion (via direct database
## read)

import
  std/[
    db_sqlite,
    sequtils,
    with,
    strutils,
    strformat,
    os,
    tables,
    times,
    typetraits,
    json
  ],
  front/[
    options,
    msgs
  ],
  ast/[
    ast,
    renderer,
    lineinfos
  ],
  utils/[
    pathutils
  ],
  ./docgen_types,
  ./docgen_text

import std/jsonutils except distinctBase
import std/sqlite3 except close
import std/options as std_options

proc newInsert(table: string, columns: openArray[(string, int)]): string =
  var r = "insert into " & table & " (\n  "
  r.add columns.mapIt(it[0]).join(", ")
  r.add "\n ) values (\n"
  for idx, (name, val) in columns:
   r.add "  ?" & $val
   if idx < columns.high:
     r.add ","

   else:
     r.add " "

   r.add " -- " & name & "\n"

  r.add ");"
  return r

proc newTable(
    table: string,
    columns: openArray[(string, string)],
    extra: string = ""
  ): SqlQuery =

  var r: string

  r.add "create table "
  r.add table
  r.add "(\n"
  for idx, (name, format) in columns:
    if idx > 0: r.add ",\n"
    r.add "  "
    r.add name
    r.add " "
    r.add format

  if 0 < len(extra):
    r.add ",\n"
    r.add extra

  r.add "\n);"

  return sql(r)

proc newTableWithInsert(
    db: DbConn,
    table: string,
    columns: openArray[((string, int), string)],
    extra: string = ""
  ): string =

  var cols: seq[(string, string)]
  var insert: seq[(string, int)]
  for (key, val) in columns:
    cols.add((key[0], val))
    insert.add(key)

  db.exec sql(&"drop table if exists {table}")

  db.exec newTable(table, cols, extra)
  result = newInsert(table, insert)


proc reset(p: SqlPrepared) =
  discard reset(p.PStmt)

proc doExec(sq: DbConn, prep: SqlPrepared) =
  sq.exec(prep)
  reset(prep)

proc toSqlite(t: typedesc[int]): string = "INTEGER"
proc toSqlite(t: typedesc[bool]): string = "INTEGER"
proc toSqlite(t: typedesc[enum]): string = "INTEGER"
proc toSqlite(t: typedesc[string]): string = "TEXT"

proc toSqlite[T](t: typedesc[Option[T]]): string =
  toSqlite(typeof Option[T]().get())

template sq(expr: untyped): untyped =
  toSqlite(typeof expr)

proc bindParam[E: enum](ps: SqlPrepared, idx: int, opt: E) =
  bindParam(ps, idx, opt.int)

proc bindParam[T](ps: SqlPrepared, idx: int, opt: Option[T]) =
  if opt.isSome():
    bindParam(ps, idx, opt.get())

proc bindParam(
    ps: SqlPrepared, idx: int,
    it: FileIndex | uint16 | int | bool
  ) =

  bindParam(ps, idx, it.int)

proc bindParam(
    ps: SqlPrepared, idx: int,
    it: DocEntryId | DocOccurId | DocLocationId | DocExtentId | DocTextId) =
  if not isNil(it):
    bindParam(ps, idx, it.int)

const
  sqPrimary = " PRIMARY KEY UNIQUE NOT NULL"
  sqNNil = " NOT NULL"

template withPrepared(conn: DbConn, prepCode: SqlPrepared, body: untyped): untyped =
  block:
    var prep {.inject.} = prepCode
    body
    finalize(prep)

template withPrepared(
    conn: DbConn, prepCode: string,
    prepName: untyped,
    body: untyped
  ): untyped =

  block:
    var prepName {.inject.} = conn.prepare(prepCode)
    body
    finalize(prepName)

template withPrepared(conn: DbConn, prepCode: string, body: untyped): untyped =
  withPrepared(conn, prepCode, prep, body)

const tab = (
  files: "files",
  entr: "entries",
  occur: "occurencies",
  loc: "locations",
  ext: "extents",
  docs: "docs",
  docmap: "docMap"
)

func refs(name: string): string = " REFERENCES " & name

func toSqlite(t: typedesc[DocEntryId]): string = sq(int) & refs(tab.entr)
func toSqlite(t: typedesc[DocOccurId]): string = sq(int) & refs(tab.occur)
func toSqlite(t: typedesc[FileIndex]): string = sq(int) & refs(tab.files)
func toSqlite(t: typedesc[DocLocationId]): string = sq(int) & refs(tab.loc)
func toSqlite(t: typedesc[DocExtentId]): string = sq(int) & refs(tab.ext)
func toSqlite(t: typedesc[DocTextId]): string = sq(int) & refs(tab.docmap)


proc writeTable*[K, V](conn: DbConn, table: Table[K, V], tabname, keyname, valname: string) =
  withPrepared(conn, conn.newTableWithInsert(tabname, {
    (keyname, 1): sq(K),
    (valname, 2): sq(V)
  })):
    for key, val in pairs(table):
      prep.bindParam(1, key)
      prep.bindParam(2, val)
      conn.doExec(prep)

proc readTable*[K, V, T](
    conn: DbConn, table: var Table[K, V], tabname: string, rows: typedesc[T]
  ) =

  for (key, val) in conn.typedRows(tabname, T):
    table[key] = val


proc writeTable*[K, V](
    conn: DbConn, table: Table[K, seq[V]],
    tabname, keyname, valname: string
  ) =

  withPrepared(conn, conn.newTableWithInsert(tabname, {
    (keyname, 1): sq(K),
    (valname, 2): sq(V)
  })):
    for key, val in pairs(table):
      for item in val:
        prep.bindParam(1, key)
        prep.bindParam(2, val)
        conn.doExec(prep)

proc readTable*[K, V, T](
    conn: DbConn, table: var Table[K, seq[V]], tabname: string, rows: typedesc[T]
  ) =
  for (key, val) in conn.typedRows(tabname, T):
    table.mgetOrPut(key).add val

proc writeSqlite*(conf: ConfigRef, db: DocDb, file: AbsoluteFile)  =
  var conn = open(file.string, "", "", "")

  conn.exec(sql"BEGIN TRANSACTION")

  conn.writeTable(db.deprecatedMsg, "deprecated", "id", "msg")

  withPrepared(conn, conn.newTableWithInsert(tab.docmap, {
    ("entry", 1): sq(DocEntryId),
    ("idx", 2): sq(DocEntryId),
    ("doc", 3): sq(DocTextId)
  })):
    for id, entry in db.entries:
      for idx, doc in entry.docs:
        prep.bindParam(1, id)
        prep.bindParam(2, idx)
        prep.bindParam(3, doc)
        conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.files, {
    ("id", 1): sq(int) & sqPrimary,
    ("abs", 2): sq(string),
    ("rel", 3): sq(string),
    ("hash", 4): sq(string)
  })):
    for idx, file in conf.m.fileInfos:
      prep.bindParam(1, idx.int)
      prep.bindParam(2, file.fullPath.string)
      prep.bindParam(3, file.projPath.string)
      prep.bindParam(4, file.hash)
      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.docs, {
    ("id", 1): sq(int) & sqPrimary,
    ("runnable", 2): sq(int),
    ("implicit", 3): sq(DocEntryId),
    ("text", 4): sq(int),
    ("location", 5): sq(int),
    ("tree", 6): sq(string)
  })):
    for id, doc in db.docs:
      prep.bindParam(1, id)
      prep.bindParam(2, doc.isRunnable)

      if doc.isRunnable:
        prep.bindParam(3, doc.implicit)

      prep.bindParam(4, doc.text)
      prep.bindParam(5, doc.location)
      if doc.tree.isNil():
        prep.bindNull(6)

      else:
        var tmp: string
        tmp.toUgly(doc.tree.toJson())
        prep.bindParam(6, tmp)

      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.entr, {
    ("id", 1): sq(int) & sqPrimary,
    ("name", 2): sq(string),
    ("kind", 3): sq(DocEntryKind),
    ("location", 4): sq(DocLocationId),
    ("extent", 5): sq(DocExtentId),
    ("parent", 6): sq(DocEntryId),
    ("condition", 7): sq(string),
    ("node", 8): sq(string)
  })):
    for id, entry in db.entries:
      with prep:
        bindParam(1, id)
        bindParam(2, entry.name)
        bindParam(3, entry.kind)

      if entry.location.isSome():
        prep.bindParam(4, entry.location.get())

      if entry.extent.isSome():
        prep.bindParam(5, entry.extent.get())

      if entry.parent.isSome():
        prep.bindParam(6, entry.parent.get())

      if 0 < entry.context.whenConditions.len():
        prep.bindParam(7, entry.context.whenConditions.mapIt(
          &"({$it})").join(" and "))

      elif 0 < entry.context.whenConditionText.len():
        prep.bindParam(7, entry.context.whenConditionText)

      else:
        prep.bindNull(7)

      if entry.kind in {ndkArg}:
        let node =
          case entry.kind:
            of ndkArg: entry.argType
            else: nil

        var str = ""
        if not isNil(node):
          str = $node

        elif entry.nodeStr.isSome():
          str = entry.nodeStr.get()

        if 0 < len(str):
          assert entry.kind == ndkArg
          assert entry.kind.int != 0
          prep.bindParam(8, str)

        else:
          prep.bindNull(8)

      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.loc, {
    ("id", 1): sq(int) & sqPrimary,
    ("file", 2): sq(int),
    ("line", 3): sq(int),
    ("col_start", 4): sq(int),
    ("col_end", 5): sq(int)
  })):
    for id, loc in db.locations:
      with prep:
        bindParam(1, id)
        bindParam(2, loc.file)
        bindParam(3, loc.line)
        bindParam(4, loc.column.a)
        bindParam(5, loc.column.b)

      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.ext, {
    ("id", 1): sq(int) & sqPrimary,
    ("file", 2): sq(FileIndex),
    ("line_start", 3): sq(int),
    ("col_start", 4): sq(int),
    ("line_end", 5): sq(int),
    ("col_end", 6): sq(int)
  })):
    for id, extent in db.extents:
      with prep:
        bindParam(1, id)
        bindParam(2, extent.file)
        bindParam(3, extent.start.line)
        bindParam(4, extent.start.column)
        bindParam(5, extent.finish.line)
        bindParam(6, extent.finish.column)

      conn.doExec(prep)

  withPrepared(conn, conn.newTableWithInsert(tab.occur, {
    ("id", 1): sq(int) & sqPrimary,
    ("kind", 2): sq(DocOccurKind),
    ("refid", 3): sq(int) & sqNNil,
    ("loc", 4): sq(DocLocationId),
    ("user", 5): sq(DocEntryId)
  })):
    for id, occur in db.occurencies:
      with prep:
        bindParam(1, id)
        bindParam(2, occur.kind)
        bindParam(3, occur.refid)
        bindParam(4, occur.loc)
        bindParam(5, occur.user)

      conn.doExec prep

  proc kindTable[E: enum](e: typedesc[E]) =
    withPrepared(conn, conn.newTableWithInsert($E, {
      ("id", 1): sq(int),
      ("name", 2): sq(string)
    })):
      for item in low(E) .. high(E):
        prep.bindParam(1, item.int)
        prep.bindParam(2, $item)
        conn.doExec(prep)

  kindTable(DocEntryKind)
  kindTable(DocOccurKind)

  conn.exec(sql"END TRANSACTION")
  conn.close()

proc getColumn(row: InstantRow, value: var float, idx: int) =
  value = column_double(row, idx.int32)

proc getColumn(row: InstantRow, value: var string, idx: int) =
  value = $column_text(row, idx.int32)

proc getColumn[I: SomeInteger](row: InstantRow, value: var I, idx: int) =
  value = I(column_int(row, idx.int32))

proc getColumn(row: InstantRow, value: var bool, idx: int) =
  value = bool(column_int(row, idx.int32))

proc getColumn[T: distinct](row: InstantRow, value: var T, idx: int) =
  var tmp: distinctBase(typeof(value))
  getColumn(row, tmp, idx)
  value = typeof(value)(tmp)

proc getColumn[T](row: InstantRow, value: var Option[T], idx: int) =
  if column_type(row, idx.int32) != SQLITE_NULL:
    var tmp: T
    getColumn(row, tmp, idx)
    value = some tmp

  else:
    value = none(T)

proc getColumn[E: enum](col: InstantRow, value: var E, idx: int) =
  var tmp: int
  getColumn(col, tmp, idx)
  value = E(tmp)

iterator typedRows[T: tuple](
    conn: DbConn, query: SqlQuery | SqlPrepared,
    types: typedesc[T]): T =

  for row in instantRows(conn, query):
    var res: T
    var idx = 0
    for field, value in fieldPairs(res):
      getColumn(row, value, idx)
      inc idx

    yield res

iterator typedRows[T: tuple](
    conn: DbConn, table: string, types: typedesc[T]): T =

  when not isNamedTuple(types):
    {.error: "`typedRows` expects named tuple elements".}

  var query = "SELECT "
  var idx = 0
  var tmp: T
  for name, value in fieldPairs(tmp):
    if 0 < idx: query.add ", "
    query.add name
    inc idx

  query.add " FROM " & table & ";"

  for res in typedRows(conn, sql(query), types):
    yield res

proc readSqlite*(conf: ConfigRef, db: var DocDb, file: AbsoluteFile) =
  var conn = open(file.string, "", "", "")
  for row in conn.typedRows(tab.files, tuple[
    id: FileIndex,
    abs: string,
    rel: string,
    hash: string
  ]):
    conf.m.fileInfos.add TFileInfo(
      fullPath: row.abs.AbsoluteFile,
      projPath: row.rel.RelativeFile,
      hash: row.hash
    )

    doAssert row.id.int == conf.m.fileInfos.high

  conn.readTable(
    db.deprecatedMsg, "deprecated", tuple[id: DocEntryId, msg: string])

  for (id, name, kind, loc, ext, parent, condition, node) in conn.typedRows(tab.entr, tuple[
    id: DocEntryId,
    name: string,
    kind: DocEntryKind,
    location: Option[DocLocationId],
    extent: Option[DocExtentId],
    parent: Option[DocEntryId],
    condition: string,
    node: Option[string]
  ]):
    doAssert id == db.add(DocEntry(
      name: name,
      kind: kind,
      location: loc,
      extent: ext,
      parent: parent,
      nodeStr: node,
      context: DocDeclarationContext(whenConditionText: condition)
    ))

    if parent.isSome():
      db[parent.get()].nested.add id

  for (id, file, line, a, b) in conn.typedRows(tab.loc, tuple[
    id: DocLocationId,
    file: FileIndex,
    line: int,
    col_start: int,
    col_end: int
  ]):
    doAssert id == db.add(DocLocation(
      file: file,
      line: line,
      column: a..b
    ))

  for (id, file, la, ca, lb, cb) in conn.typedRows(tab.ext, tuple[
    id: DocExtentId,
    file: FileIndex,
    line_start: int,
    col_start: int,
    line_end: int,
    col_end: int
  ]):
    doAssert id == db.add(DocExtent(
      file: file,
      start: (la, ca),
      finish: (lb, cb)
    ))

  for (id, kind, refid, loc, user) in conn.typedRows(tab.occur, tuple[
    id: DocOccurId,
    kind: DocOccurKind,
    refid: DocEntryId,
    loc: DocLocationId,
    user: DocEntryId
  ]):
    doAssert id == db.add(DocOccur(
      kind: kind,
      loc: loc,
      refid: refid,
      user: user
    ))

  for (id, text, runnable, implicit, location, tree) in conn.typedRows(tab.docs, tuple[
    id: DocTextId,
    text: string,
    runnable: bool,
    implicit: Option[DocEntryId],
    location: DocLocationId,
    tree: Option[string]
  ]):
    var doc = DocText(
      text: text, isRunnable: runnable, location: location)

    if runnable:
      doc.implicit = implicit.get()

    if tree.isSome():
      doc.tree = DocTextTree()
      doc.tree.fromJson(tree.get().parseJson())

    doAssert id == db.add(doc)

  for (id, doc) in conn.typedRows(tab.docmap, tuple[
    entry: DocEntryId,
    doc: DocTextId
  ]):
    db[id].docs.add doc

  close(conn)
