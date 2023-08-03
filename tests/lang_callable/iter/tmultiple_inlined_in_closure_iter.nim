discard """
  targets: "c js vm"
  description: '''
    Using the same inline iterator multiple times in the context of a
    *top-level* closure iterator must be possible and produce the
    correct code
  '''
"""

iterator simple(): int =
  var i = 0 # each inlining must produce a new instance of the local
  while i < 2:
    yield i
    inc i

iterator iter(): (int, int) {.closure.} =
  # test that the inline iterator's locals are not shared by nesting
  # their usage. If the locals are shared, then the outer loop would only
  # run once, since `i` would be 2 after the inner loop finished iterating
  for a in simple():
    for b in simple():
      yield (a, b)

let it = iter
# test that all possible pairs are returned in the correct order
doAssert it() == (0, 0)
doAssert it() == (0, 1)
doAssert it() == (1, 0)
doAssert it() == (1, 1)
discard it()
doAssert finished(it)