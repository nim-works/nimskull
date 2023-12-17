discard """
  description: '''
    Regression test for the compiler crashing when there's an error in a
    module and the error limit is not 1
  '''
  targets: native
  errormsg: "type mismatch: got <int literal(1)> but expected 'string"
  matrix: "--errorMax:100"
"""

# the error itself doesn't matter, it only matter that there's an error in
# top-level code
var a: string = 1