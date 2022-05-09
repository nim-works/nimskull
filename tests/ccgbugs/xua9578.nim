import t9578_unchecked_array

proc testUncheckedArray*(x: var UncheckedArray[mytype]) =
  f(x[0].addr)

proc testUncheckedArray2*(x: var ptr UncheckedArray[mytype]) =
  f(x[0].addr)
