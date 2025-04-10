discard """
  description: '''
    Ensures that noreturn calls work when appearing as a tailing expression of
    a .tailcall body
  '''
"""

proc noreturn() {.noreturn.} =
  raise ValueError.newException("")

# simple case: no implicit cleanup
proc test(cond: bool): int {.tailcall.} =
  if cond: 1
  else:    noreturn()

discard test(true)

# more complex: with implicit cleanup
type Obj = object
proc `=copy`(x: var Obj, y: Obj) = discard

proc test(cond: bool, x: sink Obj): int {.tailcall.} =
  if cond: 1
  else:    noreturn()

discard test(true, Obj())
