discard """
description: '''
Test inline iterators, `break`, `continue`, and other control-flow controls
for the for loop. Returning values from the iterators, order of control-flow
transfer bettween body of the iterator and main loop.
'''
targets: "!vm"
"""

# knownIssue: try/finally in the VM is broken in non-simple cases

## An iterator is similar to a procedure, except that it can be called in
## the context of a `for` loop. Iterators provide a way to specify the
## iteration over an abstract type. The yield statement in the called
## iterator plays a key role in the execution of a for loop. Whenever a
## yield statement is reached, the data is bound to the for loop variables
## and control continues in the body of the for loop. The iterator's local
## variables and execution state are automatically saved between calls.

block just_yield:
  iterator yieldExample(): int =
    yield 12

  ## Single `yield` call will result in a single for loop execution
  for value in yieldExample():
    doAssert value == 12

## In order to show how control flow is switched between iterator
## body and the loop itelf we will use a global sequence that we push
## elements into, and then assert that they were pushed in the correct order.
## For example - two yields with elements added in the `for` loop body
## itself.
block order_assertion_example:
  var values: seq[string]

  iterator orderExample(): int =
    ## Iterators can access variables outside of the loop just like regular
    ## procedures.
    values.add "started iterator"
    yield 12
    values.add "after yield"
    yield 24
    values.add "another yield"

  for value in orderExample():
    values.add "got " & $value & " from the iterator"

  doAssert values == @[
    "started iterator",
    "got 12 from the iterator",
    "after yield",
    "got 24 from the iterator",
    "another yield"
  ]

block iterator_state_preservation:
  ## When an iterator is executed, its whole state is preserved,
  ## even when control flow is transferred to the `for` loop body.
  var values: seq[string]

  iterator storeState(): int =
    var state = "starting state"
    yield 1
    values.add state
    ## Changing internal state of the iterator here - "after yield"
    ## should be placed in values after the element from the `for` loop
    ## body.
    state = "after yield"
    yield 2
    values.add state

  for item in storeState():
    values.add "got " & $item

  doAssert values == @[
    "got 1",
    "starting state",
    "got 2",
    "after yield"
  ]

  ## As you can see, the list of values contains the result of the first
  ## yield, the original state, then second yield and then the value of the changed
  ## state; this was despite changing the state before the second yield, but adding
  ## it after the second yield.

## It is still possible to use loop control flow constructs - `break` and
## `continue`.
block iterator_and_break:
  block clean_break:
    ## When break occurs in the `for` loop, control no longer returns to
    ## the iterator body.
    var values: seq[string]
    iterator breakIn(): int =
      values.add "before 1"
      yield 1
      values.add "after 1"

      values.add "before 2"
      yield 2

      ## If `break` is called while result of the `yield 2` is being
      ## processed then this statement won't be executed.
      values.add "after 2"

    for value in breakIn():
      values.add $value
      if value == 2:
        break

    doAssert values == @[
      "before 1",
      "1",
      "after 1",
      "before 2",
      "2"
    ]

  block break_try_finally:
    ## If an iterator maintains an internal state that has to be 'cleaned' up
    ## `try: ... finally:` expressions can be used to allow resources to be
    ## handled even if a `break` call is made during the `try` body.

    var values: seq[string]
    iterator breakFinally(): int =
      values.add "resource setup"

      try:
        values.add "before 1"
        yield 1
        ## If break is called, control flow won't return to this part
        ## of the code and it will not be executed
        values.add "after 1"

      finally:
        values.add "resource cleanup"

    for item in breakFinally():
      values.add $item
      break

    doAssert values == @[
      "resource setup",
      "before 1",
      "1",
      "resource cleanup"
    ]

    ## If 'break' is not called `finally` branch is still executed
    values = @[]

    for item in breakFinally():
      values.add $item

    doAssert values == @[
      "resource setup",
      "before 1",
      "1",
      "after 1",
      "resource cleanup"
    ]
