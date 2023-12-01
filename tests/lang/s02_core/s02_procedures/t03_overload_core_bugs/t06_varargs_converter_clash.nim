discard """
description: '''
`varargs[int]` should take precedence over `varargs[C]` even if `int` is
implicitly convertible to the `C` via user-defined converter.
'''
knownIssue: "Varargs clashes with converter"
"""


type
  C = object

converter toC(i: int): C = C()

## Should work because each argument in varargs has exact match which
## takes precedence over converter match.
proc impl(args: varargs[C]): string = "C"
proc impl(args: varargs[int]): string = "int"

doAssert impl(1, 2, 3, 4) == "int"
# XXX: current error: "ambiguous call; both t06_varargs_converter_clash.impl(args: varargs[C]) [proc declared in t06_varargs_converter_clash.nim(17, 6)] and t06_varargs_converter_clash.impl(args: varargs[int]) [proc declared in t06_varargs_converter_clash.nim(18, 6)] match for: (int literal(1), int literal(2), int literal(3), int literal(4))"
