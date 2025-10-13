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

# for this test, both procedures must:
# * share the same user-provided name
# * create an anonymous environment object with the exact same shape and field
#   names

proc outer(x: int) =
  var x = 1
  proc inner() =
    x = 2
  inner()

proc outer(x: float) =
  var x = 1
  proc inner() =
    x = 2
  inner()

outer(1)
outer(1.0)
