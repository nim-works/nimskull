discard """
  targets: native
  errormsg: "inheritance only works with non-final objects"
  file: "tparameterizedparent1.nim"
  line: 15
"""
# bug #5264
type
  FruitBase = object
    color: int

  Apple[T] = object of T
    width: int

var x: Apple[FruitBase]
