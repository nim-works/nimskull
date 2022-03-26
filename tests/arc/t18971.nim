discard """
  matrix: "--gc:arc"
"""

type MyObj = ref object

var o = MyObj()
proc x: var MyObj = o

var o2 = x()
