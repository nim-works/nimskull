import tcodegen_generic_openarray_import

proc testOpenArray*(x: var openArray[mytype]) =
  f(x[0].addr)

