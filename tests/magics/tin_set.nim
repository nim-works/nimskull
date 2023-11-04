discard """
  targets: "c js vm"
  description: '''
    Tests for the special semantics of the ``in`` operator with regards to
    range checks
  '''
"""

var a: int = -1
# `a` is an int, and an implicit range check is thus used. However,
# instead of failing at run-time
doAssert a notin {0, 1}

# it works the same for non-set-constructions
var se = {range[0..2](0), 1}
doAssert a notin se

# explicit conversions are currently affected to
var b: int = -1
doAssert range[0..2](b) notin {range[0..2](0), 2}

# if using a set construction as the set operand, the substituting
# element operands are always evaluated, even if the in-range test fails
var i = 0
doAssert a notin {0, 1, (inc i; 2)}
doAssert i == 1