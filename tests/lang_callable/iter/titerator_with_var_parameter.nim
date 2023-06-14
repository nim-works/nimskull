discard """
  labels: "var_args"
  description: '''
    Tests for iterators used with for loops, where the iterator has a `var`
    parameter
  '''
"""

block modification_visibility:
  # iterator parameters can use ``var``, in which case the argument must be a
  # lvalue expression of something mutable. Mutations from inside the iterator
  # are visible to the callsite
  iterator iter(a: var int): int {.inline.} =
    doAssert a == 1 # the correct value is observable in the iterator's body
    inc a
    yield a
    doAssert a == 3 # the modification from the loop's body is visible here

  var value = 1
  for i in iter(value):
    doAssert value == 2 # the changes must be visible here
    inc value

  doAssert value == 3 # here too

block modification_visibility_2:
  # mutations from inside the iterator's body are also visible on the
  # source location when not yielding prior. In general, the changes must
  # be visible at the source location whenever control-flow leaves the
  # iterator immediate body
  var g {.global.} = 1

  proc call() =
    doAssert g == 2

  iterator iter(a: var int): int {.inline.} =
    inc a
    call()
    yield a

  for i in iter(g):
    doAssert g == 2

  doAssert g == 2

block argument_has_side_effect:
  # the effects of an argument passed to a ``var`` parameter are only computed
  # once when the iterator is invoked
  iterator iter(a: var int): int {.inline.} =
    # use the parameter multiple times
    inc a
    inc a
    inc a
    yield a

  var
    value = 1
    counter = 0
  for i in iter((inc counter; value)):
    doAssert i == 4

  doAssert value == 4
  doAssert counter == 1, "effects of arguments computed more than once"
