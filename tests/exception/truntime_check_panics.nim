discard """
  description: '''
    Ensure that the compiler-inserted run-time checks don't have exceptional
    exits when panics are enabled.
  '''
  targets: native
  matrix: "--panics:on --hints:off --expandArc:test"
  action: compile
  nimout: '''
--expandArc: test
scope:
  def a: array[0..0, int]
  chckIndex(arg a, arg i)
  discard a[i]
  chckBounds(arg a, arg 0, arg i)
  def _6: openArray[int] = toOpenArray a, 0, i
  def _7: int = addI(arg i, arg i)
  def _8: int = unaryMinusI(arg i)
  def _9: range 0..1(int) = chckRange(arg i, arg 0, arg 1)
  chckField(arg <D0>, arg o.kind, arg false, arg "field \'x\' is not accessible for type \'Object\' using \'kind = ")
  discard o.kind.x
  def _11: bool = isNil(arg r)
  def _10: bool = not(arg _11)
  if _10:
    chckObj(arg r, arg type(Sub:ObjectType))
  discard r.(Sub)
  def _12: float = mulF64(arg f, arg f)
  chckNaN(arg _12)

-- end of expandArc ------------------------'''
"""

# make sure all run-time checks are enabled
{.push boundChecks: on, overflowChecks: on, rangeChecks: on, objChecks: on,
       fieldChecks: on, infChecks: on, nanChecks: on.}

type
  Sub = ref object of RootObj
  Object = object
    case kind: bool
    of true:
      x: int
    else:
      discard

# export the procedure so that it's not omitted
proc test(i: int, f: float, o: Object, r: ref RootObj) {.exportc.} =
  var a: array[1, int]
  discard a[i]                 # index check
  discard toOpenArray(a, 0, i) # bound check
  discard i + i                # overflow check for binary arithmetic
  discard -i                   # overflow check for unary arithmetic
  discard range[0..1](i)       # range check
  discard o.x                  # field check
  discard Sub(r)               # object check
  discard f * f                # infinity and nan check

{.pop.}