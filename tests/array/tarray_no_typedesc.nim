discard """
cmd: "nim check $file"
errormsg: "invalid type: 'typedesc[int]' in this context: 'array[0..0, typedesc[int]]' for var"
nimout: '''
tarray_no_typedesc.nim(15, 5) Error: invalid type: 'type' in this context: 'array[0..0, type]' for var
tarray_no_typedesc.nim(16, 5) Error: invalid type: 'typedesc[int]' in this context: 'array[0..0, typedesc[int]]' for var
'''
description: '''
  . From https://github.com/nim-lang/Nim/pull/13261,
    https://github.com/nim-lang/Nim/issues/7331,
    https://github.com/nim-lang/Nim/issues/9932,
  . Compiler should crash when using typedesc inside of array.
'''
"""
var y: array[1,type]
var x = [int]