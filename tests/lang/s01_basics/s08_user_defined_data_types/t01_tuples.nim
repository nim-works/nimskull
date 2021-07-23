discard """
description: '''
Tuples refer to a broadset of types, each of which has zero or more fields but
the total fields and their order, names, and individual types are fixed at time
of definition. Tuples are structurally matched, meaning two tuple types that
have the same fields (in order, name, and type) are considered the same.

Covers:
- literals
- anonymous tuples
- type definitions
- indexing
- type match/mismatch
'''
"""

block tuple_anon_literals:
  let
    empty = ()
    single = (1,)        # need a trailing `,` to disambiguate from parentheses
    pair = (2, "string")
    triple = (3, "foo", ("bar", "baz"))

  doAssert empty == (),           "compare element-wise - empty"
  doAssert single == (1,),        "compare element-wise - single"
  doAssert single != (2,),        "compare element-wise - single unequal"
  doAssert pair == (2, "string"), "compare element-wise - pair"
  doAssert triple == (3, "foo", ("bar", "baz")),
    "compare element-wise - triple"

# xxx: moar tests
