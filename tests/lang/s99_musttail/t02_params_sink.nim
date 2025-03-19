discard """
  description: "`sink` parameters are fully supported by `.musttail` routines"
"""

type Data = object
  # user-defined data type with custom copy behaviour
  val: int

proc `=copy`(a: var Data, b: Data) =
  a = b

proc testm(a: sink Data): Data {.musttail.} =
  a

# arguments can be locals (when movable):
proc test(): Data =
  var x = Data(val: 3)
  testm(x)

doAssert test() == Data(val: 3)

# arguments can be temporaries:
proc testTemp(): Data =
  proc temp(): Data = Data(val: 1)

  testm(temp())

doAssert testTemp() == Data(val: 1)

# arguments can be constants:
proc testConstants(): Data =
  testm(Data(val: 1))

doAssert testConstants() == Data(val: 1)
