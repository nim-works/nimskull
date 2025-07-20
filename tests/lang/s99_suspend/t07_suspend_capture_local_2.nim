discard """
  description: '''
    Non-wwning locals are not captured upon being used in the suspend block.
  '''
  output: ""
"""

type Object = object

proc `=copy`(a: var Object, b: Object) =
  echo "copied"

proc use(x: Object) = discard

proc test() =
  var x {.cursor.} = Object()
  suspend void, cont:
    use(x)
  use(x)

test()
