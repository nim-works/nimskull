discard """
  output: "ok"
  targets: "c js vm"
  matrix: "--overflowchecks:off"
  description: "Test the ability to detect overflows"
  knownIssue.vm: "bound checks are always performed"
"""

{.push overflowChecks: on.}

var
  a = high(int)
  b = -2
  r: int
  overflowDetected = false

try:
  r = (b - a)
except OverflowDefect:
  overflowDetected = true

{.pop.} # overflow check

# XXX: overflow checks (and other checks) cannot be disabled for module-level
#      code at the moment
doAssert overflowDetected == false, "re-enable this test, module-level code overflow checking now works"

block: # Overflow checks in a proc
  var
    a = high(int)
    b = -2
    overflowDetected = false

  {.push overflowChecks: on.}
  proc foo() =
    let c = b - a
  {.pop.}

  try:
    foo()
  except OverflowDefect:
    overflowDetected = true

  doAssert(overflowDetected)

block: # Overflow checks in a forward declared proc
  var
    a = high(int)
    b = -2
    overflowDetected = false

  proc foo()

  {.push overflowChecks: on.}
  proc foo() =
    let c = b - a
  {.pop.}

  try:
    foo()
  except OverflowDefect:
    overflowDetected = true

  doAssert(overflowDetected)

block: # Overflow checks doesn't affect fwd declaration
  var
    a = high(int)
    b = -2
    overflowDetected = false

  {.push overflowChecks: on.}
  proc foo()
  {.pop.}

  proc foo() =
    let c = b - a

  try:
    foo()
  except OverflowDefect:
    overflowDetected = true

  doAssert(not overflowDetected)


echo "ok"
