discard """
  targets: "c js"
  description: '''
    A |NimSkull| constant can act as an alias for an external, constant
    entity
  '''
"""

type Obj = object # something complex that isn't inlined
  val: int

const
  constA {.exportc.} = Obj(val: 2)
  constB {.importc: "constA", nodecl.} = Obj(val: 1)

static:
  # in a compile-time context, the constant represents the value as is
  # specified by the constant's initializer expression
  doAssert constB == Obj(val: 1)

# in a run-time context, the constant acts as an alias for the imported
# entity
doAssert constB == Obj(val: 2)