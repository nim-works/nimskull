import tcodegen_generic_openarray_import

proc testArray*(x: var array[3,mytype]) =
  f(x[0].addr)

proc testArray2*(x: var ptr array[3,mytype]) =
  f(x[0].addr)

