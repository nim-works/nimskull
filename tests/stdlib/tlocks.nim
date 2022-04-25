discard """
  targets: "c !cpp js"
  matrix: "--threads:on"
"""

# xxx: this should work in CPP, it's a knownIssue, see PR:
#      https://github.com/nim-works/nimskull/pull/290

#bug #6049
import uselocks

var m = createMyType[int]()
doAssert m.use() == 3
