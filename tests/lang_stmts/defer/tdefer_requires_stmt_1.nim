discard """
  description: "Defer body must be statement"
  errormsg: "'defer' takes a 'void' expression"
  line: 8
"""

block:
  defer:
    true # this needs to be used