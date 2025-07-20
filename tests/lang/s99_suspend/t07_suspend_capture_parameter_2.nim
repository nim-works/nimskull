discard """
  description: '''
    Owning parameters are copied/moved into the suspend block upon use.
  '''
"""

type Object = object
  val: int

proc `=copy`(a: var Object, b: Object) =
  a.val = b.val + 1

proc use(x: Object) = discard

proc test(x: sink Object): Object =
  suspend void, cont:
    # `x` needs to be captured by-copy because it's saved into the continuation
    # upon suspending
    return x
  use(x)

doAssert test(Object(val: 1)) == Object(val: 2)
