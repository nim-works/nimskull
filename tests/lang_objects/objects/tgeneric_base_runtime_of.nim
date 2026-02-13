discard """
  description: '''
    Tests to make sure the run-time 'of' works correctly when the base type
    is generic.
  '''
  targets: "c js vm"
"""

type
  Base[T] = object of RootObj
    x: T
  DerivedA = object of Base[int]
  DerivedB = object of Base[float]

var a: ref RootObj = new(DerivedA)
var b: ref RootObj = new(DerivedB)

doAssert a of DerivedA
doAssert a of Base[int]
doAssert not(a of DerivedB)
doAssert not(a of Base[float])

doAssert b of DerivedB
doAssert b of Base[float]
doAssert not(b of DerivedA)
doAssert not(b of Base[int])
