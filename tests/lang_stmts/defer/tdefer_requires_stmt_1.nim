discard """
  description: "Defer body must be statement"
  errormsg: "expression 'true' is of type 'bool' and has to be used (or discarded)"
  line: 9
"""

block:
  defer:
    true # this needs to be used