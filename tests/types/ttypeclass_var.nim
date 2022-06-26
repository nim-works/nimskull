discard """
errormsg: "invalid type: 'object' for var"
line: 14
description: '''
  . From https://github.com/nim-lang/Nim/issues/6969
    Internal error reported when var is defined as enum

  . From https://github.com/nim-lang/Nim/issues/6461
    Trying to use object or tuple` typeclass as a variable's type
    crashes compiler
  . result in "Error: internal error: getTypeDescAux(tyBuiltInTypeClass)".
'''
"""
var a: object