discard """
description: '''
When `i` is used in the template body converter is executed, but
range conversion check is not performed, which is inconsistent.
'''
knownIssue: "https://github.com/nim-lang/Nim/issues/12780"

"""


var executed = 0
converter toInt(s: string): int =
  inc executed

block:
  template implNoUse(i: int): untyped = discard
  implNoUse("str")

  doAssert executed == 0

  template implUse(i: int): untyped = discard i
  implUse("str")

  doAssert executed == 1

block:
  template implNoUse(i: range[0 .. 5]): untyped = discard
  implNoUse(100)

  template implUse(i: range[0 .. 5]): untyped = i
  let val = implUse(100)\

  try:
    let val = 100
    discard range[0..5](val)
    doAssert false, "Range conversion should raise range defect"

  except RangeDefect:
    discard