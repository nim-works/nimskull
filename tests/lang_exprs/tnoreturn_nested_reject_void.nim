discard """
  description: '''
    Ensure nested non-noreturn statements aren't treated as such
  '''
  errormsg: "expression '1' is of type 'int literal(1)' and has to be used (or discarded)"
  line: 11
"""

let x =
  if true:
    1
  else:
    if true:
      echo "fun" # void
    else:
      raise newException(Defect, "gone") # no return