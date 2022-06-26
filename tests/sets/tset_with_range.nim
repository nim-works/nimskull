# bug #15435
discard """
errormsg: "type mismatch: got <set[uint8], set[range 1..5(uint8)]>"
nimout: '''tset_with_range.nim(28, 13) Error: type mismatch: got <set[uint8], set[range 1..5(uint8)]>
but expected one of:
proc `<`[T](x, y: set[T]): bool
  first type mismatch at position: 2
  required type for y: set[T]
  but expression 'x' is of type: set[range 1..5(uint8)]
20 other mismatching symbols have been suppressed; compile with --showAllMismatches:on to see them

expression: {1'u8, 5} < x'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/15435
    Codegen error when operating on sets of a type of a range of the same type
  . You get a codegen error instead of a type mismatch.
'''
"""

## line 20
var
  x: set[range[1u8..5u8]]

x.incl(1)
x.incl(3)
x.incl(5)

if {1u8, 5} < x:
  echo "successful"

