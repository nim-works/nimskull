discard """
  targets: "c js vm"
  description: '''
    A ``for`` construct opens a new scope (in terms of location lifetimes) for
    its body.
  '''
"""

type
  Object = object
    ## An instance records the step `e` when it goes out of scope (i.e., is
    ## destroyed).
    e: int

var steps: seq[int] ## used to track in which order things happen

proc step(n: int) =
  ## Records an entry into ``steps``.
  steps.add n

proc `=sink`(x: var Object, y: Object) =
  doAssert false,
    "a value is moved into a location that might already store a value"

proc `=destroy`(x: var Object) =
  if x.e != 0:
    step(x.e)

template test(expect: untyped, body: untyped) =
  ## Executes body in the context of a procedure and verifies that ``steps``
  ## matches `expect`.
  proc p() =
    steps.setLen(0)
    body
    doAssert steps == expect, "got: " & $steps

  p()

template iteratorTest() {.dirty.} =
  ## The general body used across all test cases. Step 0 is always the first,
  ## step 4 the last, and when executing the for-loop body, the sequence
  ## of steps must be: 1, 3, 2.
  step(0)
  for i in iter():
    step(1)
    var o = Object(e: 2)
    step(3)
    # `o`'s lifetime ends immediately when control-flow exits the for-loop's
    # body
  step(4)

block single_yield:
  ## the most simple case: an iterator made of only a single yield statement
  iterator iter(): int {.inline.} =
    step(-1)
    yield 0
    step(-2)

  test([0, -1, 1, 3, 2, -2, 4], iteratorTest)

block multi_yield:
  # the behaviour regarding lifetimes is the same with multiple ``yield``s as
  # it is with a single ``yield``
  iterator iter(): int {.inline.} =
    step(-1)
    yield 0
    step(-2)
    yield 0
    step(-3)

  test([0, -1, 1, 3, 2, -2, 1, 3, 2, -3, 4], iteratorTest)

block yield_in_loop:
  ## a ``yield`` statement inside a loop also work the same
  iterator iter(): int {.inline.} =
    step(-1)
    var x = 0
    while x < 2: # two iterations
      step(-2)
      yield x
      step(-3)
      inc x
    step(-4)

  test([0, -1, -2, 1, 3, 2, -3, -2, 1, 3, 2, -3, -4, 4], iteratorTest)

## Lifetime and scoping of for-vars:
## Their lifetime starts at the entry of the for-loop body's scope
## and ends once control-flow exits the scope.

block:
  # single yield statement
  iterator iter(): Object =
    step(-1)
    yield Object(e: -2)
    step(-3)

  test([0, -1, 1, 3, 2, -2, -3, 4], iteratorTest)

block:
  # two yield statements
  iterator iter(): Object =
    step(-1)
    yield Object(e: -2)
    step(-3)
    yield Object(e: -4)
    step(-5)

  test([0, -1, 1, 3, 2, -2, -3, 1, 3, 2, -4, -5, 4], iteratorTest)

block:
  # yield statement in loop
  iterator iter(): Object =
    step(-1)
    var x = 0
    while x < 2:
      step(-2)
      yield Object(e: -3)
      step(-4)
      inc x
    step(-5)

  test([0, -1, -2, 1, 3, 2, -3, -4, -2, 1, 3, 2, -3, -4, -5, 4], iteratorTest)