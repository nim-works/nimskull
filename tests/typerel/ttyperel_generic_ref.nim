discard """
errormsg: "type mismatch: got <Thin[system.int]>"
nimout: '''ttyperel_generic_ref.nim(25, 6) Error: type mismatch: got <Thin[system.int]>
but expected one of:
proc test[T](x: Paper[T])
  first type mismatch at position: 1
  required type for x: Paper[test.T]
  but expression 'tn' is of type: Thin[system.int]

expression: test tn'''
description: '''
  . From https://github.com/nim-lang/Nim/issues/7600
    generic object descended from generic ref object sigmatch bug
'''
"""

type
  Paper[T] = ref object of RootObj
    thickness: T
  Thin[T]  = object of Paper[T]

proc test[T](x: Paper[T]) = discard

var tn = Thin[int]()
test tn