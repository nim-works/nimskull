discard """
targets: "c"
description: '''
Emit pragma outputs code directly through the back, these are C examples.
'''
"""

block emit_type:
  {.emit: """/*TYPESECTION*/
struct CStruct { int field; };
""".}

  type
    CStruct {.importc: "struct CStruct".} = object
      field: cint


  var struct = CStruct()
  struct.field = 12

block interpolate_variables:
  proc impl() = 
    var nimValue: cint = 0

    doAssert nimValue == 0
    {.emit: [nimValue, " += 2;"].}

    doAssert nimValue == 2

    {.emit: "`nimValue` += 2;".}
    doAssert nimValue == 4

  impl()