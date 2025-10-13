discard """
  description: '''
    Ensure for-loop vars are usable in call expressions when the iterable slot
    has an error.
  '''
  matrix: "--errorMax:100"
  errormsg: "undeclared identifier: 'unknown'"
  line: 11
"""

for x in unknown:
  echo x
