discard """
targets: "c cpp"
output: '''@[(v: -1), (v: 2), (v: 3)]
@[(v: -1), (v: 2), (v: 3)]
'''
"""

type mytype* = object
  v:int

proc f*(x:ptr mytype) = x.v = -1

func g(x:int):mytype = mytype(v:x)

import xua9578
block:
  var x = @[1.g,2.g,3.g]
  var y = cast[ptr UncheckedArray[mytype]](addr x[0])
  testUncheckedArray(y[])
  echo x
block:
  var x = @[1.g,2.g,3.g]
  var y = cast[ptr UncheckedArray[mytype]](addr x[0])
  testUncheckedArray2(y)
  echo x
