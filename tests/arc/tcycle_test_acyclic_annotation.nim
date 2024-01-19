discard """
  description: "A variation of ``tcycle_test`` where ``.acyclic`` is used"
  targets: "c"
  matrix: "-d:useMalloc"
  valgrind: true
"""

type
  Bridge {.acyclic.} = ref object
    other: Cycle
  Cycle = ref object
    bridge: Bridge
    self: Cycle

proc init() =
  # create two cycles and link them together via `Bridge`
  var a = Cycle(self: Cycle())
  a.self.self = a
  var c = Cycle(self: Cycle())
  c.self.self = c

  a.self.bridge = Bridge(other: c)
  # the following structure now exists:
  # A -> B -> Bridge -> C -> D
  # ^    |              ^    |
  # + -- +              + -- +

  b = nil # remove the stack root, which marks A as a potential cycle root
  # 'A' is now the root of a garbage cycle

GC_disableOrc() # disable the cycle collector

var s: seq[Cycle]
# register enough potential cycle roots for the default collection
# threshold (128) to overflow
for i in 0..<1000:
  var a = Cycle()
  s.add a
  # force the `add` to copy by using `a` afterwards. The subsequent destroy
  # for the ref will add it to the list of potential cycle roots
  discard a

init() # setup the dangerous cycle
# first enable the GC again. This is important, as it sets the root list
# threshold back to its default value
GC_enableOrc()
# now force a garbage collection, which results in a recursive cycle collector
# invocation and thus memory corruption.
GC_fullCollect()