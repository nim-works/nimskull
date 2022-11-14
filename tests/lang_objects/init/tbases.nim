discard """
  description: "Test the initialization of type fields in parent objects"
  matrix: "--gc:arc; --gc:refc"
  targets: c js
"""

type
  Inh = object of RootObj

  A = object of RootObj
    x: Inh
  B = object of A
    y: Inh
  C = object of B
    z: Inh

proc test[T](x: ptr RootObj, t: typedesc[T]): bool =
  x of T

proc test(v: ptr C) =
  doAssert test(addr v.x, Inh)
  doAssert test(addr v.y, Inh)
  doAssert test(addr v.z, Inh)
  doAssert test(v, C) # make sure that the type header is set correctly

var v = C() # test with non-ref
test(addr v)

var v2 = new(C) # test with new'ed ref
test(addr v2[])

var v3 = (ref C)() # test with implicit `new`
test(addr v3[])

proc p() =
  var v = C() # test with non-global
  test(addr v)

p()