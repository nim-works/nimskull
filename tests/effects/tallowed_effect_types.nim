discard """
  description: '''
    Specification-like test for covering what types are allowed in .raises and
    .tags lists
  '''
  target: native
"""

type
  ImproperTag = object
  ProperTag = object of RootEffect

  ImproperException = object of RootObj
  ProperException = object of CatchableError

# proper exception types are usable in a .raises effect list, including with
# a single ref or ptr indirection
doAssert compiles(proc() {.raises: [ProperException].})
doAssert compiles(proc() {.raises: [ref ProperException].})
doAssert compiles(proc() {.raises: [ptr ProperException].})

# multiple ref or ptr indirections are not allowed
doAssert not compiles(proc() {.raises: [ref ref ProperException].})
doAssert not compiles(proc() {.raises: [ptr ptr ProperException].})
doAssert not compiles(proc() {.raises: [ptr ref ProperException].})

# arbitrary object types are allowed in .raises effect lists too
# TODO: disallow these
doAssert compiles(proc() {.raises: [ImproperException].})
doAssert compiles(proc() {.raises: [ptr ImproperException].})
doAssert compiles(proc() {.raises: [ref ImproperException].})

# non-object types are not allowed in .raises effect lists
doAssert not compiles(proc() {.raises: [int].})
doAssert not compiles(proc() {.raises: [ref int].})
doAssert not compiles(proc() {.raises: [ptr int].})

# proper tag types are usable in a .tags effect list, including with a single
# ref or ptr indirection
doAssert compiles(proc() {.tags: [ProperTag].})
doAssert compiles(proc() {.tags: [ref ProperTag].})
doAssert compiles(proc() {.tags: [ptr ProperTag].})

# multiple ref or ptr indirections are not allowed
doAssert not compiles(proc() {.tags: [ref ref ProperTag].})
doAssert not compiles(proc() {.tags: [ptr ptr ProperTag].})
doAssert not compiles(proc() {.tags: [ptr ref ProperTag].})

# arbitrary object types are allowed in .tags effect lists too
doAssert compiles(proc() {.tags: [ImproperTag].})
doAssert compiles(proc() {.tags: [ref ImproperTag].})
doAssert compiles(proc() {.tags: [ptr ImproperTag].})

# non-object types are not allowed in .tags effect lists
doAssert not compiles(proc() {.tags: [int].})
doAssert not compiles(proc() {.tags: [ref int].})
doAssert not compiles(proc() {.tags: [ptr int].})
