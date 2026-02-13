discard """
  description: '''
    Imported C struct types can be used in compile-time evaluation contexts.
  '''
  targets: "c"
  joinable: false
"""

type Extern {.importc: "struct NoTypedef", header: "t01_c_interop.nim.h".} = object
  field1: cint
  field2: cint
  # not all fields need to exposed

## The external type is usable in a compile-time evaluation context.

proc make(val: int): Extern {.compiletime.} =
  result = Extern(field1: cint(val))

## It's also usable for the type of constants, whose value may cross the
## compile-/run-time boundary.

const c1 = make(1)
const c2 = (a: Extern(field1: 1, field2: 2), b: 3)

doAssert c1.field1 == 1
doAssert c1.field2 == 0

doAssert c2.a.field1 == 1
doAssert c2.a.field2 == 2
doAssert c2.b == 3
