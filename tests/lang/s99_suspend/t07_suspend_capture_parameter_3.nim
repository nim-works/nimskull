discard """
  description: '''
    Owning parameters not used in a suspend block are not captured by it.
  '''
  output: ""
"""

type Object = object
  val: int

proc `=copy`(a: var Object, b: Object) =
  echo "copied"

proc use(x: Object) = discard

proc test(x: sink Object) =
  # `x` is not copied because it is not used in the suspend block
  suspend void, cont:
    return
  use(x)

test(Object(val: 1))
