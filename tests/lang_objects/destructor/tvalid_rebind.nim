discard """
  targets: "c js vm"
  description: '''
    Ensure that it's possible to explicitly bind a type-bound operator to
    both the generic type and a specific instance thereof
  '''
  knownIssue: "No copy of the type is created for ``Phantom[int]``"
"""

type
  Phantom[T] = object
    ## A type where the generic parameter is not used in the body

proc `=destroy`(x: var Phantom[int]) =
  discard

# explicitly binding an operator to the generic type works and doesn't
# affect the one bound to the specific instance

proc `=destroy`[T](x: var Phantom[T]) =
  doAssert false

proc test() =
  var x = Phantom[int]()

test()
