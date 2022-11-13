discard """
  errormsg: "type mismatch between pattern '$i' (position: 1) and HourRange var 'hour'"
  file: "strscans.nim"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/8925
      scanf Invalid node kind nnkBracketExpr for macros.$
    . The issue is that in principle the call to scanf tries to fail,
      but crashes when printing the error message.
    . https://github.com/Vindaar/Nim/commit/e9addb3ab2bdcc37581f1c053d9cfa62bf86d1c0
      fix nim-lang#8925 by using getTypeInst instead of getType
  '''
"""

import strscans

type
  HourRange = range[0..23]

var
  hour: HourRange
  timeStr: string

if scanf(timeStr, "$i", hour):
  discard

