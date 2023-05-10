discard """
  matrix: "--gc:arc"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/5342
    Enable manual destructor invocation for implicitly generated destructors
  . Right now, it does not appear to be possible to manually invoke the destructor
   for a type when that destructor is defined implicitly, ie,
   it contains a destructable type
   '''
"""


type
  A = object
    id: int
  B = object
    a: A

var inputs = newSeq[int]()
proc `=destroy`(a: var A) = inputs.add( a.id )

proc testScope() =
  var x = A(id: 1)
  var y = B(a: A(id: 2))
  `=destroy`(x)
  `=destroy`(y)
  inputs.add(3)

testScope()

doAssert inputs == @[ 1, 2, 3, 2, 1 ]
# We expect the destroys to be called -
# First in the order we manually invoke them, and then
# in the reverse order the vars are declared.
# This explains the necessity of the procedure: We need a scope to end to
# ensure that the destructor is called.