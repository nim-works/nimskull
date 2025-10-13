discard """
  description: '''
    Ensure that calling ``GC_fullCollect`` during destruction of a cell part
    of a reference cycle doesn't cause issues (stack overflow or heap
    corruption).
  '''
  targets: "c"
  matrix: "-d:useMalloc"
  valgrind: true
"""

type
  CyclicRef = ref Cyclic
  Cyclic = object
    self: CyclicRef

var destroyCalled = true

proc `=destroy`(x: var Cyclic) =
  destroyCalled = true
  GC_fullCollect()
  `=destroy`(x.self)

proc test1(a: sink CyclicRef, cond: bool) =
  # try with normal destruction. The ref is passed from the outside so that
  # the optimizer doesn't know where it came from
  a.self = a
  if cond: # obfuscate the reset for the optimizer
    a.self = nil
    # ^^ registers the cell as a potential cycle root, but effectively breaks
    # the cycle

test1(CyclicRef(), true)
doAssert destroyCalled

proc test2(a: sink CyclicRef) =
  # now test with a real cycle, where destruction is triggered by the cycle
  # collector
  a.self = a
  # XXX: work around compiler bug, by forcing a copy
  discard a

destroyCalled = false
test2(CyclicRef())
doAssert not destroyCalled
# now run the cycle collector, which should destroy the cell
GC_fullCollect()
doAssert destroyCalled