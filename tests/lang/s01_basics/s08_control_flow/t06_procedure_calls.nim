## This specifices how control flow is transferred in the procedure body.
## It also shows how exceptions, `defer` and `return` affect the control
## flow. Note - even though exceptions are used in this section. it is
## **not** a specification on how exceptions in general are to be used -
## (used-defined exceptions, coercion to a parent exception types in `as`
## blocks, exception message, stack trace and so on).

block simple_procedure:
  var values: seq[string]

  proc impl() =
    values.add "in proc"

  values.add "before proc"
  impl()
  values.add "after proc"

  doAssert values == @[
    "before proc",
    "in proc",
    "after proc"
  ]

block call_other:
  var values: seq[string]

  proc impl2() =
    values.add "in-in proc"

  proc impl1() =
    values.add "in-before proc"
    impl2()
    values.add "in-after proc"

  impl1()

  doAssert values == @[
    "in-before proc",
    "in-in proc",
    "in-after proc",
  ]

block self_recursive:
  var values: seq[string]
  var counter = 0

  proc recurse() =
    let depth = $counter
    values.add "pre recurse " & depth

    inc counter
    if counter < 3:
      recurse()

    values.add "post recurse " & depth

  recurse()

  doAssert values == @[
    "pre recurse 0",
    "pre recurse 1",
    "pre recurse 2",
    "post recurse 2",
    "post recurse 1",
    "post recurse 0",
  ]

block single_defer:
  var values: seq[string]

  proc impl() =
    defer:
      values.add "defer"

    values.add "body"

  impl()
  doAssert values == @["body", "defer"]

block multiple_defers:
  ## `defer` statements are executed in reverse order of their encontering
  ## in procedure body.
  var values: seq[string]
  proc impl() =
    defer: values.add "defer 1"
    defer: values.add "defer 2"
    defer:
      ## Nesting does not affect defer - code will be executed in regular
      ## (sequential) manner.
      values.add "defer 3"
      defer:
        values.add "defer 4"

    values.add "body"

  impl()

  doAssert values == @[
    "body",
    "defer 3",
    "defer 4",
    "defer 2",
    "defer 1",
  ]

block exception:
  var values: seq[string]

  proc raised() =
    values.add "before"
    raise (ref CatchableError)()
    values.add "after"

  try: raised() except: discard

  doAssert values == @["before"]



block call_other_exceptino:
  var values: seq[string]

  proc impl2() =
    values.add "in-in proc"
    raise (ref CatchableError)()

  proc impl1() =
    values.add "in-before proc"
    impl2()
    values.add "in-after proc"

  try: impl1() except: discard

  doAssert values == @[
    "in-before proc",
    "in-in proc",
  ]

block self_recursive_raise:
  var values: seq[string]
  var counter = 0

  proc recurse() =
    let depth = $counter
    values.add "pre recurse " & depth

    inc counter
    if counter < 3:
      recurse()

    else:
      raise (ref CatchableError)()

    values.add "post recurse " & depth

  try: recurse() except: discard

  doAssert values == @[
    "pre recurse 0",
    "pre recurse 1",
    "pre recurse 2"
  ]
