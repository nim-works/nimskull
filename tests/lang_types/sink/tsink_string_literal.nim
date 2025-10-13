discard """
  description: '''
    Ensure string character data is not modified when passing a string literal
    to a sink parameter
  '''
  targets: "c js vm"
"""

proc f_sink(x: sink string) =
  x[0] = '0'

# pass a string literal directly to a sink parameter:
f_sink("abc")

# the underlying constant string data storage must not have been
# modified:
proc test(x: string) =
  doAssert x[0] == 'a'

test("abc")
