discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/3734
    void type in object type causes internal error
  . https://github.com/nim-lang/Nim/pull/10144
    void object fields are now ignored by codegen and fields/fieldPairs iterator
'''
"""

type
  Application = object
      config: void
      i: int
      f: void

proc printFields(rec: Application) =
  for k, v in fieldPairs(rec):
    doAssert k == "i"
    doAssert v == 0

var app: Application

printFields(app)

