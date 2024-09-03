discard """
  description: '''
    Ensure that generic procedure symbols are wrapped in an error, when used
    in an argument context
  '''
  action: reject
  matrix: "--errorMax:100"
"""

proc call(p: proc(x: int)) = discard

proc withError[T](x: T) =
  missing

call(withError) # <- this crashed the compiler
