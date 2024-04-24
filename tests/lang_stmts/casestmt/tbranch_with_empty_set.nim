discard """
  description: '''
    Regression test for where an empty set/array value in a label position
    counted towards the number of required labels, leading to errors being
    reported for valid case statements.
  '''
  action: compile
"""

var x: bool

case x
of true, []: # would cause "not all cases are covered"
  discard
of false:
  discard

case x
of true, {}: # would cause "not all cases are covered"
  discard
of false:
  discard
