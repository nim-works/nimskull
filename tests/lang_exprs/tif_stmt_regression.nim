discard """
  description: '''
    Regression test for ``if`` statements within statement-list expressions.
    They were not being processed by constant folding
  '''
"""

var x = (
  # unfolded 'true'/'false' symbols lead to an internal compiler error,
  # which is used for detecting whether folding took place here
  if true:
    # branch content doesn't matter, as long as it's a statement
    var y = false
  elif true: # also test an elif branch
    var y = false
  else:
    var y = false
  1
)