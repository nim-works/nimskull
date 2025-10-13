discard """
  description: '''
    An error for violating the explicit `.raises` specification is preferred
    over the error that hooks cannot raise
  '''
  errormsg: "doRaise() can raise an unlisted exception: ref CatchableError"
  line: 16
"""

type Obj = object

proc doRaise() =
  raise CatchableError.newException("")

proc `=copy`(a: var Obj, b: Obj) {.raises: [].} =
  doRaise()
