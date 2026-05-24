discard """
  labels: "arithmetic int"
  description: '''
    Tests to make sure division works as intended.
  '''
  knownIssue.vm: "Defects cannot be caught, not even for testing purpose"
"""


block divUint64:
  proc divTest() =
    let x1 = 12'u16
    let y = x1 div 5'u16
    let x2 = 1345567'u32
    let z = x2 div 5'u32
    let a = 1345567'u64 div uint64(x1)
    doAssert y == 2
    doAssert z == 269113
    doAssert a == 112130

  divTest()

block unsigned_integer_division_by_zero:
  proc op[T](a, b: T): T {.noinline.} = a div b

  doAssertRaises DivByZeroDefect:
    discard op(1'u8, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u16, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u32, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u64, 0)

block unsigned_integer_modulo_by_zero:
  proc op[T](a, b: T): T {.noinline.} = a mod b

  doAssertRaises DivByZeroDefect:
    discard op(1'u8, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u16, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u32, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u, 0)
  doAssertRaises DivByZeroDefect:
    discard op(1'u64, 0)
