discard """
description: "Ensures untyped params aren't followed by typed ones"
cmd: "nim check --hints:off $file"
nimout: '''
tuntyped_param_not_followed_by_typed.nim(15, 29) Warning: non-untyped param must not appear after an untyped param [rsemUntypedParamsFollwedByMoreSpecificType]
tuntyped_param_not_followed_by_typed.nim(18, 37) Warning: non-untyped param must not appear after an untyped param [rsemUntypedParamsFollwedByMoreSpecificType]
tuntyped_param_not_followed_by_typed.nim(21, 46) Warning: non-untyped param must not appear after an untyped param [rsemUntypedParamsFollwedByMoreSpecificType]
'''
"""





template foo(a: untyped, b: int) =
  discard

template foo(a: int, b: untyped, c: int) =
  discard

template foo(a: int, b: varargs[untyped], c: int) =
  discard