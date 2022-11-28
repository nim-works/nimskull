discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/8231
    strutils:formatSize() is broken in JS mode
  . Echos 2.293GiB for C mode but 37568YiB for JS mode.
  . I've investigated a bit, and it seems to be like that because bitwise
    operators in JS are 32-bit, so 1 << 40 gives 256 and not 1099511627776
  . Changing bitwise move operation to mul and div. ; "
"""
import strutils

doAssert formatSize(2462056448, '.', bpIEC, false) == "2.293GiB"
