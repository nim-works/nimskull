discard """
description: '''
Ambiguous call error because explicit use of the conversion proc
is the same as a literal match
'''
errormsg: "ambiguous call; both t03_overload_core_ambiguous_varargs.impl(args: varargs[C]) [proc declared in t03_overload_core_ambiguous_varargs.nim(16, 6)] and t03_overload_core_ambiguous_varargs.impl(args: varargs[int]) [proc declared in t03_overload_core_ambiguous_varargs.nim(17, 6)] match for: (int literal(1), int literal(2), int literal(3), int literal(4))"

"""


type
  C = object

proc toC(i: int): C = C()

proc impl(args: varargs[C, toC]): string = "C"
proc impl(args: varargs[int]): string = "int"

## Expectedly fails compilation because `varargs[C, toC]` and
## `varargs[int]` have the same precedence? Converter was explicitly
## provided in the procedure definition.
doAssert impl(1, 2, 3, 4) == "int"
