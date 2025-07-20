discard """
  description: '''
    Owning locals are copied/moved into the suspend block upon use.
  '''
  output: ""
"""

type Object = object

proc `=copy`(a: var Object, b: Object) =
  echo "copied"

proc use(x: Object) = discard

proc test() =
  var x = Object()
  # `x` is not copied because it is not used in the suspend block
  suspend void, cont:
    return
  use(x)

test()
