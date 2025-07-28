discard """
  action: reject
"""

type Object = object

proc `=destroy`(x: var Object) =
  discard

proc p(): Object {.tailcall.} = Object()

proc test(): Object {.tailcall.} =
  result = Object()
  # `result` has to be destroyed at the end of the scope, preventing the call
  # from being a tail call
  return p()

discard test()
