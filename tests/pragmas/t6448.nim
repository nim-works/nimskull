discard """
  errormsg: '''ambiguous call'''
  line: 10
  disabled: "32bit"
"""

import foobar, barfoo
import macros

proc bar() {.async.} =
  echo 42

proc foo() {.async.} =
  bar()

foo()
