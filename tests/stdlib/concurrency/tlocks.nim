discard """
  targets: "c js"
  matrix: "--threads:on"
"""

#bug #6049
import uselocks

var m = createMyType[int]()
doAssert m.use() == 3
