discard """
  targets: "js"
description: '''
  . From https://github.com/nim-lang/Nim/issues/11353
    Local set generates a global set on JS backend
  . Current Output
    {}
    {1}
'''
"""

proc foo() =
  var bar: set[int16] = {}
  doAssert bar == {}
  bar.incl(1)

foo()
foo()

