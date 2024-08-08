discard """
  description: '''
    Regression test for a location's value being moved even though it's used
    afterwards.
    Refer to https://github.com/nim-works/nimskull/issues/1303
  '''
  targets: c js vm
"""

type Object = object
  has: bool

var wasDestroyed = false

proc `=destroy`(x: var Object) =
  if x.has:
    inc wasDestroyed

proc f_sink(x: sink Object) =
  # disarm the destructor:
  x.has = false

proc test() =
  var o = Object(has: true)
  try:
    try:
      f_sink(o) # `o` must not be sunken
      raise CatchableError.newException("")
    except IOError:
      # omitting the inner try made the code work as expected
      discard "not reached"
  except CatchableError:
    doAssert not wasDestroyed
    discard o # use `o`, preventing it from being moved earlier

test()
doAssert wasDestroyed
