discard """
  targets: "c js vm"
  outputsub: "value out of range: -1 notin 0 .. 5"
  exitcode: "1"
  knownIssue.js vm: '''
    correctly raises an exception, but the error message doesn't match
  '''
"""
var x: distinct range[0..5]
dec(x)