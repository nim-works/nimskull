discard """
  targets: "c js vm"
  matrix: "--experimental:views"
  description: "Regression test for a C code-generator bug"
"""

var s = @[1, 2]
for i in 0..1:
  # the C code-generator previously default initialized the global by
  # zero intializing the pointed-to seq. This is incorrect (the pointer
  # itself needs to be zero-ed, not what it points to) and caused an
  # access violation error at runtime
  var v: var seq[int] = s
  v[i] = i

doAssert s == [0, 1]