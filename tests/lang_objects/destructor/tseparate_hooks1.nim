discard """
  description: '''
    Ensure that separate hooks are created for ``ref T`` types where T are non-
    top-level object types sharing the exact same name and shape
  '''
  targets: "c js vm"
"""

# XXX: this currently relies on the backend C compiler complaining. Eventually,
#      the test should inspect the MIR output and make sure two different
#      destroy hooks are used

block:
  type Nested = object

  var a = (ref Nested)()

block:
  type Nested = object

  var b = (ref Nested)()
