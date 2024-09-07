discard """
  description: '''
    Ensure nested non-noreturn statements aren't treated as such
  '''
  errormsg: "expression '10' is of type 'int literal(10)' and has to be used (or discarded)"
  line: 11
"""

let x =
  if true:
    10
  else:
    try:
      discard
    except:
      discard