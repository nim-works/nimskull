discard """
description: '''
`{.warning.}` pragma test
'''

nimout: '''
t04_warning.nim(17, 10) Warning: User-provided warning message [User]
t04_warning.nim(22, 5) template/generic instantiation of `gen1` from here
t04_warning.nim(20, 12) Warning: gen1 size of the argument is 8 [User]
t04_warning.nim(23, 5) template/generic instantiation of `gen1` from here
t04_warning.nim(20, 12) Warning: gen1 size of the argument is 8 [User]
'''


"""

{.warning: "User-provided warning message".}

proc gen1[T](arg: T) =
  {.warning: "gen1 size of the argument is " & $sizeof(arg).}

gen1[int64](1)
gen1[float](1.0)
