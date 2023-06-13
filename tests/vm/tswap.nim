discard """
  targets: "vm"
"""

# https://github.com/nim-lang/nim/issues/2946

proc testSwap(): int =
  type T = object
    data: seq[int]
  var x: T
  x.data = @[10]
  var y = @[11]
  swap(y, x.data)
  doAssert x.data == @[11]
  doAssert y == @[10]
  result = 99

discard testSwap()

# https://github.com/nim-lang/nim/issues/15463
block:
  static:
    var s = @[1, 2, 3]
    swap(s[0], s[2])

    doAssert s == [3, 2, 1]
