discard """
  errormsg: "expected ordinal value for array index, got '\"string\"'"
  line: 10
  column: 10
  labels: "array constructor index"
  description: '''
    . Array indices need to be ordinal values.
  '''
"""
let x = ["string": 0, "index": 1]
