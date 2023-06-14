discard """
  labels: "openarray"
  description: '''
    Tests for iterators used with `for` loops, where the iterators has an
    `openArray` parameter
  '''
"""

# XXX: only inline iterators are tested here right now, but closure iterator
#      should be too!

block openarray_parameter_name_parameter:
  # ``openArray`´ parameters work the same as they do for normal procedures
  iterator iter(a: openArray[int]): int {.inline.} =
    yield a[0]

  var s = [1, 2]
  for i in iter(s):
    doAssert i == 1

block openarray_parameter_unnamed_value:
  # passing the result of a literal array-construction expression works. It is
  # evaluated once and at the *callsite*.
  var other {.global.} = 0

  iterator iter(a: openArray[int]): int =
    doAssert other == 1, "side-effects weren't computed yet"
    doAssert a == [2, 3]
    yield a[0]

  # pass something that has side-effects
  for i in iter([(inc other; 2), 3]):
    doAssert i == 2

  doAssert other == 1

block openarray_parameter_no_copy:
  # no copy of the argument is created when the argument expression has side-
  # effects
  var other {.global.} = 0

  iterator iter[T](a: openArray[T]): int {.inline.} =
    doAssert other == 1
    doAssert a[0].value == 1
    yield a[0].value

  type NoCopy = object
    value: int

  proc `=copy`(x: var NoCopy, y: NoCopy) {.error.}

  # ---- main part
  var s = @[NoCopy(value: 1)]
  for i in iter((inc other; s)):
    doAssert i == 1

  # use `s` again in order to prevent it being moved when the iterator is
  # invoked
  discard s