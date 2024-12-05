discard """
  description: '''
    Ensure that not explicitly scoped locals/temporaries in `.global`
    intializer expressions are destroyed.
  '''
  targets: c js vm
  output: "destroy: 1"
"""

type Object = object
  val: int

proc `=destroy`(x: var Object) =
  echo "destroy: ", x.val

proc test() =
  # the local (`x`) has no explicit scope
  var g {.global.} = (var x = Object(val: 1); x.val)

test()
