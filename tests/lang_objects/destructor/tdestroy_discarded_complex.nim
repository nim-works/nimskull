discard """
  description: '''
    Ensure that the result of complex expressions is properly destroyed when
    the full expression is discarded
  '''
  targets: "c js vm"
"""

type Object = object
  has: bool

var destroyed = 0

proc `=destroy`(x: var Object) =
  if x.has:
    inc destroyed

proc make(): Object = Object(has: true)

# ---------------
# test procedures

proc test_block() =
  discard (block: make())

proc test_if(cond: bool) =
  discard (if cond: make() else: make())

proc test_case(cond: bool) =
  discard (
    case cond
    of true:  make()
    of false: make()
  )

proc test_try() =
  discard (
    try:    make()
    except: make()
  )

test_block()
doAssert destroyed == 1
test_if(true)
doAssert destroyed == 2
test_case(true)
doAssert destroyed == 3
test_try()
doAssert destroyed == 4
