discard """
  targets: "c cpp"
"""

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

## A `finally` clause is executed when control-flow leaves the corresponding try clause and, if present, any executed catch clause.

block finally_simple:
  var values: seq[string]

  values.add("before")
  
  try:
    values.add("try")
  finally:
    ## When a `finally` clause is present, the `try` statement isn't required to have any `except` clauses.
    values.add("finally")

  values.add("end")
  
  doAssert values == @[
    "before",
    "try",
    "finally",
    "end"
  ]


block finally_return:
  var values: seq[string]
  
  proc impl1() =
    values.add("before")
    try:
      values.add("try")
      return
    finally:
      ## Even though the `try`-clause is left via a return 
      ## statement, the finally clause is still executed
      values.add("finally")

    values.add("end")

  try: impl1() except: discard

  doAssert values == @[
    "before",
    "try",
    "finally"
  ]

block finally_raise:
  var values: seq[string]
  
  proc impl1() =
    values.add("before")

    try:
      values.add("try")
      raise (ref CatchableError)()
      values.add("end try")
    finally:
      values.add("finally")
    
    values.add("end")

  try: impl1() except: discard

  doAssert values == @[
    "before",
    "try",
    "finally"
  ]

block simple_except_finally:
  var values: seq[string]
  
  proc impl1() =
    values.add("before")

    try:
      values.add("try")
      raise (ref CatchableError)()
      values.add("end try")
    except:
      values.add("except")
    finally:
      ## The `finally` clause is executed after any present `except` clause
      values.add("finally")
    
    values.add("end")

  impl1()

  doAssert values == @[
    "before",
    "try",
    "except",
    "finally",
    "end"
  ]


block nested_finally:
  var values: seq[string]

  proc impl2() =
    values.add("enter impl2")
    try:
      raise (ref CatchableError)()
    finally:
      values.add("finally impl2")

    values.add("leave impl2")

  proc impl1() =
    values.add("enter impl1")
    try:
      impl2()
    except:
      values.add("except")
    finally:
      values.add("finally impl1")
    
    values.add("leave impl2")

  impl1()

  doAssert values == @[
    "enter impl1",
    "enter impl2",
    "finally impl2",
    "except",
    "finally impl1",
    "leave impl2"
  ]

block nested_finally2:
  var values: seq[string]

  proc impl2() =
    values.add("enter impl2")
    try:
      raise (ref CatchableError)()
    finally:
      values.add("finally impl2")

    values.add("leave impl2")

  proc impl1() =
    values.add("enter impl1")
    try:
      impl2()
    finally:
      values.add("finally impl1")
    
    values.add("leave impl2")

  try: impl1() except: discard

  doAssert values == @[
    "enter impl1",
    "enter impl2",
    "finally impl2",
    "finally impl1"
  ]


block reraise:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref CatchableError)()
    except:
      values.add("except")
      raise
      values.add("except end")

    values.add("end")

  try: impl1() except: discard

  doAssert values == @[
    "begin",
    "try",
    "except"
  ]

block reraise_finally:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref CatchableError)()
    except:
      values.add("except")
      raise
    finally:
      values.add("finally")

    values.add("end")

  try: impl1() except: discard

  doAssert values == @[
    "begin",
    "try",
    "except",
    "finally"
  ]


block nested_finally_return:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      try:
        raise (ref CatchableError)()
      finally:
        values.add("inner finally")
        return
    finally:
      values.add("finally")
  
  try: impl1() except: discard

  doAssert values == @[
    "begin",
    "inner finally",
    "finally"
  ]

block finally_return:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref CatchableError)()
    finally:
      values.add("finally")
      return
    values.add("end")

  try: impl1() except: values.add("except")

  doAssert values == @[
    "begin",
    "try",
    "finally",
  #  "except"
  ]


block handled_exception_in_finally:
  type ExA = object of CatchableError
  type ExB = object of CatchableError

  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref ExA)()
    finally:
      values.add("finally")
      try:
        raise (ref ExB)()
      except ExB:
        values.add("except")
      
      values.add("finally end")

    values.add("end")

  try: impl1() except: values.add("outer except")

  doAssert values == @[
    "begin",
    "try",
    "finally",
    "except",
    "finally end",
    "outer except"
  ]

block finally_reraise:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref CatchableError)()
    finally:
      values.add("finally")
      raise
      values.add("finally end")

    values.add("end")

  try: impl1() except: values.add("except")

  doAssert values == @[
    "begin",
    "try",
    "finally",
    "except"
  ]

block finally_raise:
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      try:
        values.add("try")
        raise (ref CatchableError)()
      except:
        raise
      finally:
        values.add("finally")
        try:
          values.add("try inner")
          raise
        finally:
          values.add("finally inner end")

        values.add("finally end")
    finally:
      values.add("last finally")

    values.add("end")
    

  try: impl1() except: discard

  doAssert values == @[
    "begin",
    "try",
    "finally",
    "try inner",
    "finally inner end",
    "last finally"
  ]


block finally_defer:
  
  var values: seq[string]

  proc impl1() =
    values.add("begin")
    try:
      values.add("try")
      raise (ref CatchableError)()
    finally:
      values.add("finally")
      defer: values.add("defer")
      values.add("finally 2")
    values.add("end")

  try: impl1() except: discard

  doAssert values == @[
    "begin",
    "try",
    "finally",
    "finally 2",
    "defer"
  ]