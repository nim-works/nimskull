discard """
description: '''
`{.hint.}` pragma test
'''

nimout: '''
t03_hint_pragmas.nim(15, 7) Hint: User-provided hint message [User]
t03_hint_pragmas.nim(19, 9) Hint: gen1 size of the argument is 8 [User]
t03_hint_pragmas.nim(31, 6) Hint: 'declaredButNotUsed' is declared but not used [XDeclaredButNotUsed]
'''


"""

{.hint: "User-provided hint message".}


proc gen1[T](arg: T) =
  {.hint: "gen1 size of the argument is " & $sizeof(arg).}

gen1[int64](1)

when defined(tryBrokenSpecification):
  proc gen2[T](arg: T) =
    {.line: instantiationInfo().}:
      {.hint: "gen2 size of the argument is " & $sizeof(arg).}

  gen2[int](1)


proc declaredButNotUsed() = discard

when defined(tryBrokenSpecification):
  # FIXME - does not work, hint is still emitted
  {.push hint[XDeclaredButNotUsed]:off.}

  proc declaredButNotUsedOff() = discard

  {.pop.}