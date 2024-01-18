discard """
  description: '''
    Regression test for exceptions being leaked when caught by an ``T as e``
    exception branch within a closure iterator where `T` is an object type
  '''
"""

type Ex = object of CatchableError
# for this test, it's important that `Ex` is **not** a ref type

var destroyed: int

proc `=destroy`(x: var Ex) =
  inc destroyed

iterator iter(): int {.closure.} =
  var i = 0
  # catch the exception twice for the lifted `e` to be assigned twice
  while i < 2:
    try:
      raise Ex.newException("")
    except Ex as e:
      # yield within the handler so that it's split, which (at the time of
      # writing) forces `e` to be lifted into the environment
      yield i
      inc i

proc test() =
  var it = iter
  discard it()
  discard it()
  discard it() # finish iterating
  # `it` is destroyed

test()

doAssert destroyed >= 2, "object is leaked"
doAssert destroyed == 2, "object is destroyed too often (double free?)"