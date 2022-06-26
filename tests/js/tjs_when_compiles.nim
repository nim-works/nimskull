discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/11166
    Compiler crash when using jsffi and compiles
  . Combining the jsffi module with compiles statements involving dot calls
    can crash the compiler
  . The compiler crash only happens when the compiles calls fails.
    Changing the example where the same expression is valid avoids a crash
  . The problem seems to be caused by the `.` macro"
'''
"""

import jsffi

type
  C = object
    props: int

var c: C

when not compiles(c.props):
  # Should compile.
  doAssert false


when compiles(thisObjectDefDoesntExist.props):
  # Should Not compile.
  doAssert false

