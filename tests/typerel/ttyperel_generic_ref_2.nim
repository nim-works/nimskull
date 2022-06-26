discard """
errormsg: "type mismatch: got <Thin>"
nimout: '''ttyperel_generic_ref_2.nim(24, 6) Error: type mismatch: got <Thin>
but expected one of:
proc test(x: Paper)
  first type mismatch at position: 1
  required type for x: Paper
  but expression 'tn' is of type: Thin

expression: test tn'''
description: '''
   . From https://github.com/nim-lang/Nim/issues/7600
     generic object descended from generic ref object sigmatch bug
 '''
"""

type
  Paper = ref object of RootObj
  Thin  = object of Paper

proc test(x: Paper) = discard

var tn = Thin()
test tn