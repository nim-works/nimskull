discard """
  targets: "c js vm"
  knownIssue.c js: '''
    A constructor node with a resolved type-class as the type either
    crashes the compiler or results in an internal compiler error
  '''
"""

type
  MyType = object
    val: int
  Concept = concept x
    x is MyType

# semantic analysis changes the type of the ``nkObjConstr`` node to a resolved
# ``tyUserTypeClass``, which is something not considered by ``cgirgen`` and the
# code generators
var x: Concept = MyType(val: 1)
doAssert x.val == 1