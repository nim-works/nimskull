import tcodegen_generic_openarray_import

proc testTuple*(x: var tuple[a:mytype,b:mytype,c:mytype]) =
  f(x[0].addr)

proc testTuple2*(x: var ptr tuple[a:mytype,b:mytype,c:mytype]) =
  f(x[0].addr)

