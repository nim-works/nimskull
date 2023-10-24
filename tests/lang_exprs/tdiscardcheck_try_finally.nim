discard """
description: "Ensure discard checks are done for try expressions"
errormsg: "expression '1' is of type 'int literal(1)' and has to be used (or discarded)"
line: 11
"""

## This was originally introduced as a regression test, where
## `semstmts.discardCheck` erroneously checked the `finally` branch, instead
## of the last non-`finally` branch in the `try` expression. This generated an
## incomplete report, and resulted in an NPE within `cli_reporter` when
## attempting to render it.

try:
  1
finally:
  discard 1