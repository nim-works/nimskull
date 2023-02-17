discard """
  description: '''
    Tests for tuple unpacking in the presence of types with lifetime hooks
  '''
  targets: "c js !vm"
  matrix: "--cursorInference:off"
"""

import mhelper

type Obj = object
  ## Used to track what lifetime hook calls are injected

proc `=copy`(x: var Obj, y: Obj) =
  inc numCopies

proc `=destroy`(x: var Obj) =
  inc numDestroy

test unpack_complex_expression:
  # test unpacking a tuple resulting from a complex expression
  proc prc(cond: bool) =
    # don't use a static condition; the optimizer might remove the not-taken
    # branch prior to assignment rewriting otherwise
    let (a, b) =
      if cond: (initResource(), initResource())
      else:    (initResource(), initResource())

  prc(true)
  doAssert numCopies == 0
  doAssert numDestroy == 2

test no_extra_call_for_repack:
  proc make(): tuple[a, b, c: Obj] =
    # pack a tuple. No copy nor destroy hook calls must be injected here
    (Obj(), Obj(), Obj())

  proc repack(): tuple[a, b, c: Obj] =
    # unpack and repack the tuple. No copy nor destroy hook calls must be
    # injected here
    let (a, b, c) = make()
    result = (a: a, b: b, c: c)

  proc prc() =
    discard repack()
    # the tuple is not consumed; each element must be destroyed at the end of
    # the procedure

  prc()

  doAssert numCopies == 0
  doAssert numDestroy == 3

test raise_in_constructor_expression:
  # XXX: this test is only tangentially related to tuple unpacking. It should
  #      be moved somewhere else

  proc init(): Resource =
    # the ``init`` procedure is used so that a temporary is definitely used
    result = initResource()

  proc doRaise(cond: bool): Resource =
    # a procedure that only in theory returns a resource. At run-time it always
    # raises an exception
    if cond:
      raise (ref CatchableError)()

  proc make(): (Resource, Resource) =
    # the temporary resulting from the ``init`` call must be destroyed and
    # no valid value must be observable at the callsite of ``make``
    result = (init(),
              doRaise(true))

  try:
    discard make()
  except:
    discard

  doAssert numCopies == 0
  doAssert numDestroy == 1