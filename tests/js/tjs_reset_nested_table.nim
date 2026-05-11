discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/11697
    JS backend crashes when trying to reset a Table inside a Table
'''
"""

import std/tables

var xs: Table[int, Table[int, int]]

doAssertRaises(KeyError): reset xs[0]
