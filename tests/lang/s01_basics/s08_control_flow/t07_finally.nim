discard """
  targets: "c cpp"
"""

## A finally clause is executed once control-flow leaves the `finally`'s corresponding `try` and (if present) `except clause. This 
## includes normal, interrupted (`break`, `continue`, `yield`, `return`) and exceptional (`raise`) control flow.

block:
  var values: seq[string]

  proc p() =
    block a:
      block b:
        try:
          values.add("1")
          try:
            values.add("2")
            return
          finally:
            values.add("3")
            break a
        finally:
          values.add("4")
          break b
        values.add("5")
      values.add("6")
    values.add("7")

  p()

  doAssert values == @[
    "1",
    "2",
    "3",
    "4",
    "6",
    "7",
  ]


block:
  var values: seq[string]
  block a:
    try:
      try:
        values.add("1")
        raise (ref CatchableError)()
      finally:
        values.add("2")
        break a
    except:
      values.add("3")
    finally:
      values.add("4")
    values.add("5")

  doAssert values == @[
    "1",
    "2",
    "3",
    "4"
  ]

block:
  var values: seq[string]

  block a:
    block b:
      try:
        values.add("1")
        break a
      finally:
        values.add("2")
        block c:
          try:
            values.add("3")
            break b
          finally:
            values.add("4")
            break c
          values.add("5")
        values.add("6")
    values.add("7")

  doAssert values == @[
    "1",
    "2",
    "3",
    "4",
    "6",
    "7"
  ]


block:
  var values: seq[string]
  try:
    try:
      values.add("1")
      raise (ref CatchableError)()
    finally:
      values.add("2")
      try:
        values.add("3")
        raise (ref CatchableError)()
      except:
        values.add("4")
  except:
    values.add("5")

  doAssert values == @[
    "1",
    "2",
    "3",
    "4",
    "5"
  ]


block:
  iterator iter(): int {.closure.} =
    try:
      raise (ref CatchableError)()
    finally:
      yield 1

  var it = iter
  # TODO: what should happen here?
  doAssert it() == 1

block:
  iterator iter(): int {.closure.} =
    # This closure iterator currently kills the compiler with a stack-overflow
    block:
      try:
        try:
          raise (ref CatchableError)()
        finally:
          yield 1
      finally:
        break

  var it = iter
  doAssert it() == 1

block:
  var values: seq[string]
  proc p() =
    values.add("1")
    try:
      values.add("2")
      return
    finally:
      values.add("3")
    values.add("4")

  p()

  doAssert values == @[
    "1",
    "2",
    "3",
    "4"
  ]


block return_raise_rvo:

  type Large = object
    x: int
    a: array[256, int]

  proc p(): Large =
    result.x = 0
    try:
      raise (ref Exception)()
    finally:
      return Large(x: 1)

  block:
    var a: Large
    try:
      a = p()
    except:
      discard
    doAssert a.x == 0



block return_break_rvo:

  proc p(): int =
    block:
      try:
        return 1
      finally:
        break

  block:
    var a: int
    a = p()
    doAssert a == 1


block break_and_finally:
  var values: seq[string]

  proc p() =
    try:
      block b:
        try:
          return
        finally:
          values.add "inner"
          break b

      values.add "end"
    finally:
      values.add "finally"

  p()

  doAssert values == @[
    "inner",
    "end",
    "finally"
  ]


block raise_break_and_finally:
  var values: seq[string]

  proc p() =
    try:
      block b:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "inner"
          break b

      values.add "block end"
    except:
      values.add "except"
    finally:
      values.add "finally"
    values.add("end")

  p()

  doAssert values == @[
    "inner",
    "except",
    "finally",
    "end"
  ]





block break_and_none_finally:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          break
      except:
        values.add "except"
      finally:
        values.add "finally outer"
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "finally outer",
    "end"
  ]


block continue_and_none_finally:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          continue
      except:
        values.add "except"
      finally:
        values.add "finally outer"
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "finally outer",
    "loop",
    "finally",
    "except",
    "finally outer",
    "loop",
    "finally",
    "except",
    "finally outer",
    "end"
  ]



block continue_and_none:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          continue
      except:
        values.add "except"
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "end"
  ]

block continue_and_return:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          continue
      except:
        values.add "except"
        return
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except"
  ]

block continue_and_continue:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          continue
      except:
        values.add "except"
        continue
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "end"
  ]

block continue_and_break:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          continue
      except:
        values.add "except"
        break
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "end"
  ]


block break_and_continue:
  var values: seq[string]
  proc p() =
    for i in 0..2:
      values.add "loop"
      try:
        try:
          raise (ref CatchableError)()
        finally:
          values.add "finally"
          break
      except:
        values.add "except"
        continue
      values.add $i
    values.add "end"

  p()

  doAssert values == @[
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "loop",
    "finally",
    "except",
    "end"
  ]



## Control-flow statements in a `finally` clause are able to override control-flow statements
## in the corresponding try/except clauses or finally clauses nested inside the try/except clauses

template defineBlockProc(values, inTryStmt, inFinallyStmt) {.dirty.} =
  proc p() =
    values.add("begin")
    block b:
      values.add("block")
      try:
        values.add("try")
        inTryStmt
      finally:
        values.add("finally")
        inFinallyStmt
      values.add("block end")
    values.add("end")

template blockTestCase(inTryStmt, inFinallyStmt, expect) =
  block:
    var values {.inject.}: seq[string]

    defineBlockProc(values, inTryStmt, inFinallyStmt)

    p()

    doAssert values == expect


blockTestCase():
  break
do:
  return
do:
  @[
    "begin",
    "block",
    "try",
    "finally"
  ]

blockTestCase():
  return
do:
  break
do:
  @[
    "begin",
    "block",
    "try",
    "finally",
    "end"
  ]

blockTestCase():
  break
do:
  discard
do:
  @[
    "begin",
    "block",
    "try",
    "finally",
    "end"
  ]

block finally_control_flow:
  var values: seq[string]
  
  proc p() =
    values.add("begin")
    for i in 0..2:
      values.add($i)
      try:
        break
      finally:
        values.add("finally")
        continue

    values.add("end")

  p()

  doAssert values == @[
    "begin",
    "0",
    "finally",
    "1",
    "finally",
    "2",
    "finally",
    "end"
  ]


block finally_control_flow:
  var values: seq[string]
  
  proc p() =
    values.add("begin")
    for i in 0..2:
      values.add($i)
      try:
        return
      finally:
        values.add("finally")
        continue

    values.add("end")

  p()

  doAssert values == @[
    "begin",
    "0",
    "finally",
    "1",
    "finally",
    "2",
    "finally",
    "end"
  ]



block finally_control_flow:
  var values: seq[string]
  
  proc p() =
    values.add("begin")
    for i in 0..2:
      values.add($i)
      try:
        continue
      finally:
        values.add("finally")
        break

    values.add("end")

  p()

  doAssert values == @[
    "begin",
    "0",
    "finally",
    "end"
  ]


block nested_finally_in_finally:
  blockTestCase():
    return
  do:
    try:
      values.add("inner try")
    finally:
      ## This will leave the function
      return

    values.add("finally end")
    break b
  do:
    @[
      "b",
      "inner try"
    ]



block:
  var values: seq[string]
  proc p() =
    block blo:
      try:
        try:
          values.add("1")
          return
        finally:
          values.add("2")
          return
      finally:
        values.add("3")
        break
      values.add("4")
    values.add("5")

  p()

  doAssert values == @[
    "1",
    "2",
    "3",
    "5"
  ]

block:
  var values: seq[string]
  proc a() =
    for i in 0..2:
      try:
        try:
          try:
            values.add("raise")
            raise (ref CatchableError)()
          finally:
            values.add("inner finally")
            return
        except:
          values.add("inner except")
          raise
      except:
        values.add("except")
      finally:
        values.add("finally")
      values.add($i)

    values.add("end")
  
  a()

  doAssert values == @[
    "raise",
    "inner finally",
    "inner except",
    "except",
    "finally",
    "0",

    "raise",
    "inner finally",
    "inner except",
    "except",
    "finally",
    "1",

    "raise",
    "inner finally",
    "inner except",
    "except",
    "finally",
    "2",

    "end"
  ]

block:
  var values: seq[string]
  proc a() =
    for i in 0..1:
      try:
        try:
          for j in 0..1:
            try:
              raise (ref CatchableError)()
            finally:
              values.add("in finally")
              break
            values.add("in loop")
          values.add("post inner loop")
        except:
          values.add("in except")
          raise
      except:
        values.add("except")
      finally:
        values.add("finally")
      
      values.add("loop")

    values.add("end")

  a()

  doAssert values == @[
    "in finally",
    "in except",
    "except",
    "finally",
    "loop",

    "in finally",
    "in except",
    "except",
    "finally",
    "loop",

    "end"
  ]



block:
  var values: seq[string]
  block a:
    for i in 0..1:
      try:
        break a
      finally:
        values.add("finally")
        break
      values.add($i)

    values.add("end")
  
  doAssert values == @[
    "finally",
    "end"
  ]