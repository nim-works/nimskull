discard """
  targets: "c js vm"
  description: '''
    Tests to make sure that lifted globals defined inside inline iterators work
  '''
"""

iterator iter(): (int, ptr int) =
  var global {.global.} = 1
  inc global
  yield (global, addr global)

# use the iterator in two separate procedures and check that the accessed
# global is the same in both

proc test1(): (int, ptr int) =
  for r in iter():
    return r

proc test2(): (int, ptr int) =
  for r in iter():
    return r

let (val, address) = test1()
doAssert val == 2

# invoking the iterator again must not reset the global to its initial
# value
doAssert test2() == (3, address)