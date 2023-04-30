discard """
  targets: "c js !vm"
  matrix: "--cursorInference:off"
  description: '''
    Regression test to make sure that locations using a resolved concept type
    are being considered by the ``injectdestructors`` pass
  '''
"""

# cursor inference is disable as it interferes with the test

type
  Type = object
    val: int
  TypeConcept = concept x
    x is Type

var
  copied = false
  destroyed = false

proc `=copy`(a: var Type, b: Type) =
  a.val = b.val
  copied = true

proc `=destroy`(x: var Type) =
  destroyed = true

proc test() =
  var x = Type(val: 1)
  var y: TypeConcept = x

  # use `x` again to force the earlier assignment being turned into a copy
  # instead of a sink
  doAssert x.val == 1
  doAssert y.val == 1

test()
doAssert copied
doAssert destroyed