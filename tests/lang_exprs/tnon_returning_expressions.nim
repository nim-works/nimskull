discard """
  description: '''
    Ensure that the compiler can correctly handle all forms of:
    * expression that don't return
    * statements that don't return
  '''
  targets: c js vm
"""

proc noret() {.noreturn.} =
  raise CatchableError.newException("")

# ------------------------------
# establish that the basics work

proc testBreak() =
  block:
    var x = (;break; 1)
    doAssert false

testBreak()

proc testReturn() =
  var x = (;return; 1)
  doAssert false

testReturn()

proc testRaise() =
  var x = (;raise CatchableError.newException(""); 1)
  doAssert false

try: testRaise() except: discard

proc testNoreturnCall() =
  var x = (noret(); 1)
  doAssert false

try: testNoreturnCall() except: discard

# ----------
# statements

template testStmt(body: untyped) =
  # provides the fixture for testing statements
  proc p(): int {.gensym.} =
    body
    doAssert false
  discard p()

proc call(a, b: int) = discard

# callee:
testStmt: (;return; call)(1, 2)
# arguments:
testStmt: call((;return; 1), 2)
testStmt: call((;return; var val = 0; 1), val)

# ------------------------------------
# complex statements that don't return

# discard operand doesn't return:
testStmt:
  discard (;return; 1)

# raise operand doesn't return:
testStmt:
  raise (;return; CatchableError.newException(""))

# first definition doesn't return:
testStmt:
  let
    x = (;return; var a = 2; 1)
    y = a

# second (non-trailing) definition doesn't return:
testStmt:
  let
    x = 1
    y = (;return; var a = 3; 2)
    z = a

# lhs in assignment doesn't return:
testStmt:
  var x = 0
  (;return; var y = 1; x) = y

# rhs in assignment doesn't return:
testStmt:
  var x = 0
  x = (;return; 1)

# condition expression in if doesn't return:
testStmt:
  let x = 1
  if (;return; x == 1):
    doAssert false

# condition in trailing elif-branch doesn't return:
testStmt:
  let x = 2
  if x == 1:
    doAssert false
  elif (;return; x == 2):
    doAssert false

# case statement selector expression doesn't return
testStmt:
  let x = 1
  case (;return; x)
  of 1:
    doAssert false
  else:
    doAssert false

when defined(c):
  block:
    var value = 0
    testStmt:
      {.emit: "`value` = 1;", emit: [value, "=", (;return; 2)].}

    # make sure the first emitted statement was evaluated:
    doAssert value == 1


# -----------
# expressions

template testExpr(body: untyped) =
  # provides the fixture for testing expressions
  proc p() {.gensym.} =
    let x = body
    doAssert false
  p()

var global = 0

proc effect(): int =
  # effectful procedure
  inc global
  result = 1

template testExprWithEffect(expect: int, body: untyped) =
  # provides the fixture for testing expressions where some side-effect needs
  # to be computed (or not)
  global = 0
  testExpr: body
  doAssert global == expect

type Obj = object
  a, b, c: int

# construction expressions:
testExprWithEffect 0:
  [(;return; var val = 0; 1), val, effect()]
testExprWithEffect 0:
  ((;return; var val = 0; 1), val, effect())
testExprWithEffect 0:
  Obj(a: (;return; var val = 0; 1), b: val, c: effect())

# cast:
testExprWithEffect 0:
  cast[int]((;return; effect()))

# conversion:
testExprWithEffect 0:
  int((;return; effect()))

# object field access:
testExpr:
  let o = Obj(a: 1)
  (;return; o).a

# tuple field access:
testExpr:
  let t = (1, 2)
  (;return; t)[0]

# array operand expression doesn't return:
testExprWithEffect 0:
  var s = @[0, 1]
  (;return; s)[effect()]

# index operand expression doesn't return:
testExprWithEffect 1:
  var s = @[1, 2]
  # the array operand must still be evaluated for side-effects
  (;discard effect(); s)[(;return; 1)]
