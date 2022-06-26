discard """
  errormsg: '''ambiguous call'''
  line: 16
  disabled: "32bit"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/6448
      local macro overrides imported macro silently
    . https://github.com/nim-lang/Nim/pull/8902
      Fix overload resolution for pragmas evaluation
    '''
"""

import foobar, barfoo
import macros
# foobar and barfoo also contain definitions for {.async.}
proc bar() {.async.} =
  echo 42

proc foo() {.async.} =
  bar()

foo()

