discard """
  description: '''
    A `varargs` exact match should precede one with an explicit conversion
  '''
  knownIssue: "a match without conversion is better than one with"
"""

# xxx: revisit if the implementation is too difficult

type
  C = object

proc toC(i: int): C = C()

proc impl(args: varargs[C, toC]): string = "C"
proc impl(args: varargs[int]): string = "int"

## Should pass because `varargs[C, toC]` has a lower match precedence than
## `varargs[int]`.
doAssert impl(1, 2, 3, 4) == "int"
