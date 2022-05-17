discard """
  description: "repr for `proc` types raises a runtime error"
  action: compile
  matrix: "--gc:arc"
  knownIssue: ""
"""

# With ARC, `repr_v2` is used as the `repr` implementation inside the VM. The
# issue is that the VM currently doesn't view procdeural types as compatible
# with pointer types

proc a(): proc(): int =
  var v = 0
  proc x(): int =
    return v

  result = x

static:
  var p = a()
  discard repr(p)