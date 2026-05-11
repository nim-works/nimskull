discard """
  description: '''
    Imported C struct types can be cast to other types.
  '''
  targets: "c"
  joinable: false
"""

type
  ExternBig {.importc: "struct Struct1", header: "t08_imported_struct_cast.nim.h".} = object
    field1: cint
    field2: cint
    # not all fields need to be exposed

  ExternSmall {.importc: "struct Struct2", header: "t08_imported_struct_cast.nim.h"} = object
    field1: cint

  Intern = object
    field1: cint

let v1 = ExternBig(field1: 1, field2: 2)
let v2 = cast[ExternSmall](v1)
let v3 = cast[Intern](v1)

doAssert v1.field1 == 1
doAssert v1.field2 == 2

doAssert v2.field1 == 1

doAssert v3.field1 == 1
