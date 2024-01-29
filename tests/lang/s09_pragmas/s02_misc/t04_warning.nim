discard """
description: '''
`{.warning.}` pragma test
'''

nimout: '''
t04_warning.nim(17, 10) Warning: User-provided warning message [User]
t04_warning.nim(22, 10) template/generic instantiation of `gen1` from here
t04_warning.nim(20, 12) Warning: gen1 name of the argument is int [User]
t04_warning.nim(23, 12) template/generic instantiation of `gen1` from here
t04_warning.nim(20, 12) Warning: gen1 name of the argument is float [User]
'''


"""

{.warning: "User-provided warning message".}

proc gen1[T](arg: T) =
  {.warning: "gen1 name of the argument is " & $T.}

gen1[int](1)
gen1[float](1.0)
