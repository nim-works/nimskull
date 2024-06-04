## Temporary profiling facilities.

import std/[monotimes, exitprocs, times, db_sqlite]

type Entry = object
  name: string
  count: int
  time: Duration
  alloc: AllocStats

var counter {.compileTime.} = 0
# allocate the sequence on the heap, to make sure it not gets destroyed
# prior to the exit proc being called
var storage {.noinit.}: ptr seq[Entry]

template data*(): seq[Entry] =
  storage[]

proc register(i: int, name: string): int =
  if storage.isNil:
    storage = create(seq[Entry])

  data.setLen(max(data.len, i + 1))
  data[i].name = name
  i

proc id(name: static string): int =
  # compute a unique, 0-based ID for the name
  const x = counter
  static: inc counter
  # we use a lifted global for running some ad-hoc code at startup
  let ignore {.global, used.} = register(x, name)
  result = x

# the fields are not exported :(
template alloc*(s: AllocStats): int =
  cast[ptr array[2, int]](addr s)[][0]
template dealloc*(s: AllocStats): int =
  cast[ptr array[2, int]](addr s)[][1]

proc `+=`(a: var AllocStats, b: AllocStats) {.inline.} =
  a.alloc += b.alloc
  a.dealloc += b.dealloc

proc finish(id: int, time: Duration, stats: AllocStats) =
  data[id].alloc += getAllocStats() - stats
  data[id].time += time
  inc data[id].count

template measure*(name: static string) =
  # needs to have as little overhead as possible (e.g., no costly table
  # lookups)
  let
    start = getMonoTime()
    stats = getAllocStats()

  defer: finish(id(name), getMonoTime() - start, stats)

proc dump() =
  echo "---- Measurements:"
  for it in data.items:
    if it.count > 0:
      echo "'", it.name, "' took ", (it.time.inMilliseconds.int / 1000), "s (average: ", (it.time.inMicroseconds.int / it.count / 1000), "ms runs: ", it.count, ")"
      when defined(nimAllocStats):
        echo "  allocations: ", $it.alloc

  # write to an sqlite DB, for easier analysis later on
  var db = open("profile.db", "", "", "")
  db.exec(sql"BEGIN IMMEDIATE TRANSACTION")
  try:
    db.exec(sql"CREATE TABLE IF NOT EXISTS runs (id INTEGER PRIMARY KEY, date)")
    let run = db.tryInsertID(sql"INSERT INTO runs (date) VALUES (?)", now().format("YYYY-MM-dd HH:MM:ss"))
    doAssert run != -1
    db.exec(sql"CREATE TABLE IF NOT EXISTS entries (run INTEGER, name, count, total, alloc, dealloc)")
    for it in data.items:
      db.exec(sql"INSERT INTO entries (run, name, count, total, alloc, dealloc) VALUES (?,?,?,?,?,?)",
              run, it.name, it.count, it.time.inMicroseconds.int, it.alloc.alloc, it.alloc.dealloc)
    db.exec(sql"COMMIT")
  except:
    echo "error: ", getCurrentExceptionMsg()
    db.exec(sql"ROLLBACK")
  finally:
    db.close()

addExitProc(proc() = dump())