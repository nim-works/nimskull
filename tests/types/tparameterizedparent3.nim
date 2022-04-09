discard """
  targets: native
  errormsg: "attempt to redefine: 'color'"
  file: "tparameterizedparent3.nim"
  line: 14
"""
# bug #5264
type
  FruitBase = object of RootObj
    color: int

  Apple[T] = object of T
    width: int
    color: int

var x: Apple[FruitBase]
