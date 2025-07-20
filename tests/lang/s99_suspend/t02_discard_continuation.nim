discard """
  description: '''
    A continuation isn't required to be used. If it's not, the saved context
    is destroyed once the instance goes out of scope.
  '''
  output: "destroy\n"
"""

type Object = object

proc `=destroy`(x: var Object) =
  echo "destroy"

proc test() =
  var o = Object()
  suspend void, cont:
    return
  echo "after suspend"

test()
