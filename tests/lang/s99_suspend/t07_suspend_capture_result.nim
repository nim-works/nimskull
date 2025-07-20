discard """
  description: '''
    If the `result` variable is used (both implicitly and explicitly) in the
    suspend block, it is captured too.
  '''
"""

type Object = object
  val: int

proc `=copy`(a: var Object, b: Object) =
  a.val = b.val + 1

proc use(x: Object) = discard

proc implicit(): Object =
  result = Object(val: 1)
  suspend void, cont:
    return
  use(result)

doAssert implicit() == Object(val: 2)

proc explicit(): Object =
  result = Object(val: 1)
  suspend void, cont:
    return result
  use(result)

doAssert explicit() == Object(val: 2)
