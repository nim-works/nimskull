discard """
  description: '''
    Ensure that procedure symbols are wrapped in an error, when used in an
    argument context
  '''
  action: reject
  matrix: "--errorMax:100"
"""

proc call(p: proc()) = discard

proc withError() =
  missing

call(withError) # <- this crashed the compiler
