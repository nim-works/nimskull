discard """
  description: "Defer body must be a statement, without error"
  errormsg: "type mismatch: got <bool, int literal(1)>"
  line: 13
"""

## somewhat of a regression test to see what happens when the defer body has an
## error, this was an issue encountered in CPS, where an error body was treated
## as an expression.

block:
  defer:
    true + 1