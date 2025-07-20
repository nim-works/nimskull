discard """
  description: '''
    A continuation instance is copyable. When duplicating it, all saved values
    are duplicated, in reverse declaration order.
  '''
  output: "copied: 2\ncopied: 1\n"
"""

type Object = object
  val: int

proc `=copy`(x: var Object, y: Object) =
  x.val = y.val
  echo "copied: ", x.val

proc dup[T](x: sink T) = discard
proc use[T](x: T) = discard

proc test() =
  var a = Object(val: 1)
  var b = Object(val: 2)
  suspend void, cont:
    dup(cont) # the instance is duplicated
    use(cont)

test()
