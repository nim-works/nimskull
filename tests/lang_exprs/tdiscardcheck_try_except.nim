discard """
description: "Ensure discard checks are done for try expressions"
errormsg: "expression '1' is of type 'int literal(1)' and has to be used (or discarded)"
line: 14
"""

## This was originally introduced as a regression test, where a fix to
## `semstmts.discardCheck` erroneously changed traversals in a manner where
## the test suite passed but this was not caught, except in code review.

try:
  discard
except:
  1