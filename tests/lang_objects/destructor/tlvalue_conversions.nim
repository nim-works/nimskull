discard """
  description: '''
    Tests for assignments of types with lifetime hooks where l-value
    conversions are involved
  '''
  targets: "c js vm"
  matrix: "--gc:arc --cursorInference:off"
  knownIssue.js vm: "`--gc:arc` is not supported"
"""

import mhelper

block sink_from_ref_with_conversion:
  # test that moving from a ``ref`` location works when there's a conversion in
  # between
  type
    A = ref object of RootObj
    B = ref object of A

  proc prc(cond: bool) =
    var
      ref1 = B()
      ref2 = B()

    # force a destroy call for both `ref1` and `ref2`
    if cond:
      return

    var y: A = ref1 # uses a blit copy
    y = ref2 # use the ``=sink`` hook

    doAssert y != nil

  prc(false)
  # test that all allocated object were destroyed:
  doAssert getOccupiedMem() == 0

block move_from_lvalue_conversion:
  type
    ObjA = object
    ObjB = distinct ObjA

  proc `=destroy`(x: var ObjA) =
    doAssert false

  proc `=destroy`(x: var ObjB) =
    discard "okay; do nothing"

  proc test() =
    # destructor elision must take the lvalue conversion into account. `a` must
    # not be destroyed here
    var a = ObjA()
    var b = ObjB(a) # no copy must be used here

  test()

test move_from_temporary_with_conversion:
  type
    DValue = distinct Value[int]
    Wrapper = object
      a: Value[int]

  proc `=destroy`(x: var DValue) =
    `=destroy`(Value[int] x)

  # use a procedure so that a temporary is created
  proc make(): Wrapper =
    Wrapper(a: initValue(1))

  proc prc() =
    # move out of the temporary's field
    var b = DValue(make().a)

  prc()
  doAssert numDestroy == 1