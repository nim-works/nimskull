discard """
  description: '''
    Regression test for an internal typing issue with synthesized hooks for
    objects using inheritance
  '''
  targets: c js vm
"""

type
  Parent = object of RootObj
    x: int
  Sub = object of Parent

proc `=destroy`(x: var Parent) =
  # ensure that the parameter is at least accessible at run-time
  doAssert x.x == 1

# the destroy hook for `Sub` is synthesized by the compiler

proc test() =
  var x = Sub(x: 1)

test()
