discard """
description: '''
Unsigned integer operations kept for backwards compatibility, but should be
removed from the language eventually
'''
"""

# unsigned operators treat operands as unsigned, returning original type.
# uses modulo arithmetic, so no overflows

block:
  let
    a = int.low
    b = a +% a    # this should wrap around to zero
  doAssert typeOf(b) is int, "unsigned addition result is unsigned"
  doAssert b == 0,           "unsigned integer addition works"

block:
  let
    a = int.low  # based on the binary representation the largest number
    b = int.high # one short of the the largest number
    c = a -% b
  doAssert typeOf(c) is int, "unsigned subtraction result is unsigned"
  doAssert c == 1,           "unsigned integer subtraction works"

block:
  let
    a = int.low # half the number range based on the binary representation
    b = 2
    c = a *% 2  # covers the positive range and then rolls over to 0
  doAssert typeOf(c) is int, "unsigned multiplication result is unsigned"
  doAssert c == 0,           "unsigned integer multiplication works"

block:
  let
    a = int.low
    b = 2
    c = a /% b  # effectively drops the negative and goes to half + 1 the range
  doAssert typeOf(c) is int,          "unsigned division result is unsigned"
  doAssert c == (int.high div 2 + 1), "unsigned integer division works"

block:
  let
    a = int.low
    b = int.high
    c = a %% b   # biggest mod (biggest - 1) = 1
  doAssert typeOf(c) is int, "unsigned modulo result is unsigned"
  doAssert c == 1,           "unsigned integer modulo works"

block:
  let
    a = int.high
    b = int.low  # bigger per binary representation
  doAssert a <% b,  "unsigned less than comparison"
  doAssert a <=% b and a <= a, "unsigned less than equal to comparison"
  doAssert b >% a,  "unsigned greater than comparison"
  doAssert b >=% a, "unsigned greater than equal to comparison"

block:
  let
    before = 1'i8
    after = ze(before)
  doAssert sizeof(before) == 1 and sizeof(after) == sizeof(int),
    "deprecated: `ze` extends the size of the operand to the platform int size"

block:
  let
    before = 1'i16
    after = ze(before)
  doAssert sizeof(before) == 2 and sizeof(after) == sizeof(int),
    "deprecated: `ze` extends the size of the operand to the platform int size"
