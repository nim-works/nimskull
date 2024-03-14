discard """
description: '''
Pragmas for wrapping C code in nim.
'''
targets: "c"
cmd: "nim c -r $options $file"

joinable: false
"""

{.compile("t01_c_interop.nim.c", "-DNIM_DEFINED_FLAG").}

const h = "t01_c_interop.nim.h"


block wrap_void_proc:
  proc cVoid() {.importc: "c_void", header: "t01_c_interop.nim.h".}

block wrap_prec_with_value:
  proc c_return_int(): cint {.importc, header: h.}

  doAssert c_return_int() == 12

block wrap_variadic_c_function:
  proc sumVariadic(count: cint): cint {.importc: "c_sum_variadic", varargs, header: h.}

  doAssert sumVariadic(2, 3, 1) == 3 + 1
  doAssert sumVariadic(3, 0, 0, 0) == 0 + 0 + 0

block wrap_struct_with_no_typedef:
  type
    NoTypedef {.importc: "struct NoTypedef".} = object
      field1: cint
      field2: cint
      field3 {.importc: "__field3".}: cint

  proc returnNoTypedef(f1, f2, f3: cint): NoTypedef {.importc: "c_return_no_typedef", header: h.}

  let res = returnNoTypedef(1, 2, 3)

  doAssert res.field1 == 1
  doAssert res.field2 == 2
  doAssert res.field3 == 3

block wrap_proc_without_header:
  proc compileOnly(arg: cint): cint {.importc: "c_compiled_only".}

  doAssert compileOnly(1) == 1 * 2

  proc inDefine(): cint {.importc: "c_in_define".}

  doAssert inDefine() == 228

block wrap_typedefed_struct:
  type
    WithTypedef {.importc.} = object
      field1: cint
      field2: cint
      field3 {.importc: "__field3".}: cint