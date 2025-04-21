discard """
  description: '''
    Ensure that it's possible to take the address of a sink parameter.
  '''
  targets: "c js vm"
  knownIssue.js: '''
    Changes through a ptr-to-sink-parameter don't reflect on the parameter's
    value.
  '''
"""

proc f_sink(p: sink int) =
  let x = addr p
  doAssert x[] == 1
  x[] = 2
  doAssert x[] == 2
  doAssert p == 2

f_sink(1)
