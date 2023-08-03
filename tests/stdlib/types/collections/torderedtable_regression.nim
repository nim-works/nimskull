discard """
  description: "A regression test for lazily initialized `OrderedTable`s"
  target: native
"""

include std/tables

type Key = distinct int

proc hash(x: Key): int = int(x)
proc `==`(a, b: Key): bool {.borrow.}

# choose the key so that the hash function yields a value `x` where
# `x mod internalSlotCount` equals 0. Note that this depends on an
# implementation detail of ``OrderedTable``
let zeroKey = slotsNeeded(defaultInitialSize)

block infinite_loop:
  # ``initOrderedTable`` is explicitly **not** used 
  var tbl: OrderedTable[Key, int]

  # the key matters but the value doesn't 
  tbl[Key zeroKey] = 3
  var first = true
  for k, v in tbl.pairs:
    doAssert first # reaching this assert more than one time is an error
    first = false

block cut_off:
  var tbl: OrderedTable[Key, int]
  # insert multiple items that don't use slot 0
  tbl[Key(zeroKey+1)] = 1
  tbl[Key(zeroKey+2)] = 2
  tbl[Key(zeroKey+3)] = 3
  # now insert an item that uses slot 0
  tbl[Key(zeroKey)] = 0

  doAssert tbl.len == 4 # the length is correct

  var c = 0
  for _ in tbl.keys:
    inc c
  
  doAssert c == tbl.len # but the number of items returned is not

  # create a table with the same number of items
  var tbl2: OrderedTable[Key, int]
  # insert multiple items that don't use slot 0 and for which the value
  # differs from the ones used in `tbl`
  tbl2[Key(zeroKey+1)] = 4
  tbl2[Key(zeroKey+2)] = 5
  tbl2[Key(zeroKey+3)] = 6

  # now insert an item that uses slot 0
  tbl2[Key(zeroKey)] = 0

  doAssert tbl != tbl2