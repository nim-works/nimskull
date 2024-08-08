discard """
  description: '''
    Ensure that the result variable is cleaned up when already initialized and
    the procedure exits due to a `raise`
  '''
  targets: c js vm
"""

import mhelper

proc test(doRaise: bool): Resource =
  result = initResource()
  if doRaise:
    raise CatchableError.newException("")

block no_raise:
  # make sure the result isn't destroyed too early when no exception is
  # raised
  var v = test(false)
  doAssert numDestroy == 0

doAssert numDestroy == 1
numDestroy = 0

block do_raise:
  try:
    var v = test(true)
    # `v` must not be destroyed, otherwise there'd be a double-free
  except CatchableError:
    discard

  doAssert numDestroy == 1
