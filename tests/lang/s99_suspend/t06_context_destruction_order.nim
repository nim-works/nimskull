discard """
  description: '''
    Locals saved in a continuation instance are destroyed in reverse
    declaration order when the instance is discarded.
  '''
  output: "destroy: 3\ndestroy: 2\ndestroy: 1\n"
"""

type Object = object
  val: int

proc `=destroy`(x: var Object) =
  if x.val != 0:
    echo "destroy: ", x.val

proc test(param: sink Object): Object =
  result = Object(val: 1)
  let a = Object(val: 3)
  suspend void, cont:
    return Object(val: 0)
  # `result` is saved because its value is needed by the implicit return

discard test(Object(val: 2))
