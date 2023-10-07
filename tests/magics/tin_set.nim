discard """
  targets: "c js vm"
  description: '''
    Tests for the special semantics of the ``in`` operator with regards to
    set constructions
  '''
"""

var a = -1
# ^^ a run-time value that is not part of the non-negative integer range.
# When used as the first argument of an ``in`` call or as the second one of
# an ``contains`` call, a range error would normally occur
var b = 0'u8
var c = int high(uint16) # value outside the int16 range

# rule 1: the set argument must be a literal set construction expression
# rule 2: if a constant set, the set must be larger than the target's int size
# rule 3: a) the set must use a (non `int8`) signed integer as the element type
#         or
#         b) the construction must have 8 or less items (a range counts as a
#            single item)

# if all the above are satisfied, the range check is skipped

# rule 3.a
doAssert not(int16(c) in {0'i16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10})

# rule 3.b (the set's type is a range type)
# a constant set, but with storage larger than an int (because it's a
# ``set[0..high(uint16)]``)
doAssert not(a in {0, 1})

# rule 3.b (the set is empty)
doAssert not(a in set[0..255]({}))

# rule 3.b
doAssert not(b in {range[2'u8..255'u8](5), 6'u8})

# rule 3.b (exactly 8 elements)
doAssert not(b in {range[2'u8..255'u8](5), 6..8, 10..12, 14..16, 18..20,
                   22..24, 26..28, 30..32})

# --- negative tests
# disabled for the VM, since range check failures are uncatchable there

when not defined(vm):
  doAssertRaises RangeDefect: # rule 1 is not satisfied
    var s = {0..1}
    discard a in s
  doAssertRaises RangeDefect: # rule 2 is not satisfied
    discard b in {range[2'u8..3'u8](2)}
  doAssertRaises RangeDefect: # rule 3.b is not satisfied
    # constant set, but the element type is a ``int8``
    discard int8(c) in {0'i8, 1, 2, 3, 4, 5, 6, 7, 8, 9}
  doAssertRaises RangeDefect: # rule 3 is not satisfied
    # a constant set expression, but too many elements and the element type is
    # not a signed-integer type
    discard b in {range[1'u8..255'u8](1), 2, 3, 4, 5, 6, 7, 8, 9}