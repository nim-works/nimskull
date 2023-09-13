discard """
  targets: "c js vm"
  description: '''
    Ensure that it's possible to explicitly bind a type-bound operator to
    both the generic type and a specific instance thereof
  '''
"""

type
  Object[T] = object
    ## A type where the generic parameter *is* used in the body
    x: T

proc `=destroy`(x: var Object[int]) =
  discard

# explicitly binding an operator to the generic type works and doesn't
# affect the one bound to the specific instance

proc `=destroy`[T](x: var Object[T]) =
  doAssert false

proc test() =
  var a = Object[int]()

test()
