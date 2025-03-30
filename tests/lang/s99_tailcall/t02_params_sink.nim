discard """
  description: "`sink` parameters are fully supported by `.tailcall` routines"
  knownIssue.vm: '''
    `vmgen` doesn't handle `sink` parameters properly, leading to a run-time
    access violation
  '''
"""

type Data = object
  # user-defined data type with custom copy behaviour
  val: int

proc `=copy`(a: var Data, b: Data) =
  a = b

proc testm(a: sink Data): Data {.tailcall.} =
  a

# arguments can be locals (when movable):
proc test(): Data {.tailcall.} =
  var x = Data(val: 3)
  testm(x)

doAssert test() == Data(val: 3)

# arguments can be temporaries:
proc testTemp(): Data {.tailcall.} =
  proc temp(): Data = Data(val: 1)

  testm(temp())

doAssert testTemp() == Data(val: 1)

# arguments can be constants:
proc testConstants(): Data {.tailcall.} =
  testm(Data(val: 1))

doAssert testConstants() == Data(val: 1)
