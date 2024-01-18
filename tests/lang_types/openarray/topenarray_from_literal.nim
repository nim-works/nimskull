discard """
  targets: c js vm
  description: '''
    Ensure that creating a local, immutable openArray from a string literal
    works and that reading from the openArray does too
  '''
"""

{.experimental: "views".}

proc test() =
  let x: openArray[char] = "abc"
  var y = "def"
  # regression test for the VM: the statement above led to the assertion below
  # failing
  doAssert x[1] == 'b'

test()