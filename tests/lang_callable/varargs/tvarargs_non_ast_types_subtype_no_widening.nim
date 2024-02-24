discard """
  description: "Tests for non-ast varargs subtype widening."
  errormsg: "type mismatch: got <int16, int8, uint16, array[0..2, int32]>"
  line: 17
"""

## widening is presently disallowed as the implementation complexity is too
## high, this might be revisited in the future.


block disallow_widening:
  proc foo(v: varargs[int32], values: openarray[int32]) =
    doAssert v.len == values.len
    for i, a in v.pairs:
      doAssert a == values[i], $a & " is not equal to " & $values[i]

    foo(1'i16, 1'i8, 2'u16, [1'i32, 1'i32, 2'i32])