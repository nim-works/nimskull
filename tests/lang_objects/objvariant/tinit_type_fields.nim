discard """
  description: '''Tests variant object initialization with non-pure object
                  fields in the record-case'''
  matrix: "--gc:arc"
  targets: c js
"""

type
  Inh = object of RootObj # `Inh` has a `m_type` field on non-VM back-ends

  A = object of RootObj
    case k1: bool
    of false:
      a: array[2, Inh]
    of true:
      b: int

  B = object of A
    c: (Inh, Inh)

  C = object of B
    case k2: bool
    of false:
      d: Inh # For this test, it's important that `x` is in the default
             # branch
    of true:
      e: int
      f: Inh

proc test(x: ptr RootObj): bool =
  x of Inh

proc test(v: ptr C) =
  doAssert v.e == 0
  doAssert test(addr v.a[0])
  doAssert test(addr v.a[1])
  doAssert test(addr v.c[0])
  doAssert test(addr v.c[1])
  doAssert test(addr v.f)


proc p() =
  # test with non-global
  var v = C(k2: true)
  test(addr v)

  v = C(k2: false)
  doAssert test(addr v.d)

p()

var v = C(k2: true) # test with non-ref
test(addr v)

var v2 = (ref C)(k2: true) # test with ref
test(addr v2[])