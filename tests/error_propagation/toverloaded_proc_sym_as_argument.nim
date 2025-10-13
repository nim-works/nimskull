discard """
  description: '''
    Ensure that overloaded procedure symbols are wrapped in an error, when
    used in an argument context
  '''
  action: reject
  matrix: "--errorMax:100"
"""

proc call(p: proc()) = discard

proc withError() =
  missing

proc withError(a: int) = # <- this overload is not picked below
  discard

call(withError) # <- this crashed the compiler
