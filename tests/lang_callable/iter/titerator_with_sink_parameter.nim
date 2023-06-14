discard """
  labels: "sink_param"
  description: '''
    Tests for iterators used with for loops, where the iterator has a sink
    parameter
  '''
"""

block parameter_mutability:
  # inline iterators support `sink` parameters in the same way as procedures
  iterator iter(x: sink int): int {.inline.} =
    inc x # the sink parameter can be modified
    yield x

  var a = 1
  for i in iter(a): # `a` is not modified
    doAssert i == 2
  doAssert a == 1

  # passing an integer literal also works
  for i in iter(1):
    doAssert i == 2

block transfer_ownership:
  # ownership of the location passed as the argument is transferred to the
  # parameter
  var numDestroyed {.global.} = 0

  type Object = object
    val: int

  proc `=copy`(x: var Object, y: Object) {.error.}
  proc `=sink`(x: var Object, y: Object) =
    doAssert false, "object erroenously moved via sink hook"
  proc `=destroy`(x: var Object) =
    inc numDestroyed

  # the parameter is a location that is not alive yet when the argument is
  # moved into it, so no ``=sink`` hook must be called

  iterator iter(x: sink Object): int {.inline.} =
    doAssert x.val == 1
    yield x.val

  proc test() =
    var o = Object(val: 1)
    for i in iter(o): # o gets sunken into the parameter here
      doAssert numDestroyed == 0, "destroyed too early"
      doAssert i == 1

    doAssert numDestroyed == 1

  test()
  doAssert numDestroyed == 1, "ownership wasn't transferred properly?"

block no_use:
  # the sink parameter not being explicitly used inside the iterator's body
  # has no effect on the parameter's destruction
  type Object = object

  var steps {.global.}: seq[int]

  proc `=destroy`(x: var Object) =
    steps.add 4

  iterator iter(a: sink Object): int {.inline.} =
    steps.add 1
    yield 1
    steps.add 3
    # `a` must be destroyed when exiting `iter`s scope

  steps.add 0
  for i in iter(Object()):
    doAssert i == 1
    steps.add 2
  steps.add 5

  doAssert steps == [0, 1, 2, 3, 4, 5], $steps