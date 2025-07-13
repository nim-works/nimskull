discard """
  description: "Preliminary tests for `suspend`"
  knownIssue.vm: ".tailcall doesn't work properly"
"""

# TODO: add proper tests

proc pass[C, P](cont: sink (C, P)) =
  cont[1](cont[0])

proc pass[C, R, A](x: A, cont: sink (C, proc(a: sink A, c: sink C): R {.tailcall.})): R =
  cont[1](x, cont[0])

proc testInner(x: bool) =
  ## suspend within 'if' statement.
  if x:
    var a = "first " & $x
    if x:
      var b = "second " & $x
      suspend(void, cont, pass(cont))
      echo b
    echo a

testInner(true)

proc testLoop1() =
  ## suspend used before loop.
  suspend(void, cont, pass(cont))
  var i = 0
  while i < 10:
    inc i

testLoop1()

proc testLoop2() =
  ## suspend used within loop.
  var i = 0
  while i < 10:
    suspend(void, cont, pass(cont))
    inc i

testLoop2()

proc testLoop3() =
  ## suspend used after loop.
  var i = 0
  while i < 10:
    inc i
  suspend(void, cont, pass(cont))

testLoop3()

proc orTest(cond: bool): bool =
  ## suspend in second 'or' operand.
  let val = cond or suspend(bool, cont, pass(true, cont))
  result = val

# XXX: not a good test
doAssert orTest(false)
doAssert orTest(true)

proc testOrPred(cond: bool): int =
  if cond or suspend(bool, cont, pass(false, cont)):
    result = 1
  else:
    result = 2

# XXX: also not a good test
doAssert testOrPred(false) == 1
doAssert testOrPred(true) == 2

proc ifExprTest1(cond: bool): int =
  let got =
    if cond: 1
    else:    suspend(int, cont, pass(2, cont))
  result = got

doAssert ifExprTest1(true) == 1
doAssert ifExprTest1(false) == 2

proc ifExprTest2(cond: bool): int =
  let got =
    if cond: suspend(int, cont, pass(1, cont))
    else:    suspend(int, cont, pass(2, cont))
  result = got

doAssert ifExprTest2(true) == 1
doAssert ifExprTest2(false) == 2

proc caseExprTest1(x: range[0..2]): int =
  let got =
    case x
    of 0: x
    of 1: suspend(int, cont, pass(1, cont))
    of 2: suspend(int, cont, pass(2, cont))

  result = got

doAssert caseExprTest1(0) == 0
doAssert caseExprTest1(1) == 1

proc blockExprTest1(): int =
  let got = block:
    suspend(int, cont, pass(1, cont))
  result = got

doAssert blockExprTest1() == 1

proc tryExprTest1(doRaise: bool): int =
  let got =
    try:
      if doRaise:
        raise ValueError.newException("")
      suspend(int, cont, pass(1, cont))
    except:
      2
  result = got

doAssert tryExprTest1(true) == 2
doAssert tryExprTest1(false) == 1

proc tryTest1(doRaise: bool): int =
  ## Simple case; suspend in try/except statement, where the 'except' doesn't
  ## use anything from the outside except `result`.
  try:
    result = suspend(int, cont, pass(1, cont))
    if doRaise:
      raise ValueError.newException("")
  except:
    result = 2

doAssert tryTest1(true) == 2
doAssert tryTest1(false) == 1

proc tryTest2(doRaise: bool): int =
  ## The suspend is wrapped in a try/except where the 'except' uses some
  ## outer variables.
  var res = 2
  try:
    result = suspend(int, cont, pass(1, cont))
    if doRaise:
      raise ValueError.newException("")
  except:
    result = res

doAssert tryTest2(true) == 2
doAssert tryTest2(false) == 1
