discard """
  description: '''
    A regression test for a bug with the cycle collector, where two reference
    cycles linked together by a non-cyclic reference type trigger a memory
    corruption
  '''
  targets: "c"
  matrix: "-d:useMalloc"
  valgrind: true
"""

type
  Bridge = object
    other: Cycle2
  Cycle1 = ref object
    bridge: ref Bridge
      ## bridges the two cyclic types. This must be a `ref`, otherwise tracing
      ## would visit the `other` field and the issue would not be triggered
    self: Cycle1
  Cycle2 = ref object
    self: Cycle2

proc init() =
  # create two cycles and link them together via `Bridge`
  var a = Cycle1(self: Cycle1())
  a.self.self = a
  var b = Cycle2(self: Cycle2())
  b.self.self = b

  a.self.bridge = new Bridge
  a.self.bridge.other = b
  # the following structure now exists:
  # A -> B -> Bridge -> C -> D
  # ^    |              ^    |
  # + -- +              + -- +

  b = nil # remove the stack root, which marks A as a potential cycle root
  # 'A' is now the root of a garbage cycle

GC_disableOrc() # disable the cycle collector

var s: seq[Cycle1]
# register enough potential cycle roots for the default collection
# threshold (128) to overflow
for i in 0..<1000:
  var a = Cycle1()
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