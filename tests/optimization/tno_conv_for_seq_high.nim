discard """
  description: '''
    Ensure that no to-slice conversion is emitted for `high` calls with
    `seq` arguments
  '''
  action: compile
  matrix: "--expandArc:test"
  nimout: '''--expandArc: test
scope:
  result = high(arg x)
return result

-- end of expandArc ------------------------'''
"""

proc test(x: seq[int]): int {.exportc.} =
  result = high(x)
