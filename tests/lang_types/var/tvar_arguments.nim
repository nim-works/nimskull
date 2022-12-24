discard """
  target: "c js !vm"
  labels: "var_arg"
  description: '''
    Tests to make sure arguments are properly passed to var parameter and that
    mutations through the ``var`` parameters are reflected on the source
    location
  '''
"""

# knownIssue: ``vmgen`` generates incorrect code in some cases

template disableIf(cond: bool, body: untyped) =
  when defined(tryBrokenSpecification) or not cond:
    body

proc asgn[T](a: var T, b: T) =
  a = b

proc asgnViaPtr[T](a: var T, b: T) =
  ## Assigns `b` to `a` using a pointer indirection. This tests whether taking
  ## the address of ``var`` parameters works
  let p = addr a
  p[] = b

proc testObjField[T](a, b: T) =
  # object field access
  type Obj = object
    val: T

  var obj = Obj()
  asgn(obj.val, a)
  doAssert obj.val == a

  asgnViaPtr(obj.val, b)
  doAssert obj.val == b

proc testTupleField[T](a, b: T) =
  # tuple field access
  var tup: (T, T)
  asgn(tup[0], a)
  doAssert tup == (a, default(T))

  asgnViaPtr(tup[1], b)
  doAssert tup == (a, b)

proc testSeq[T](a, b: T) =
  # passing a seq element as the argument
  const def = default(T)

  var s = @[def, def]
  asgn(s[1], a)
  doAssert s == [def, a]

  asgnViaPtr(s[1], b)
  doAssert s == [def, b]

proc testRefDeref[T](a, b: T) =
  # test passing a dereferenced ``ref`` as the argument
  var r: ref T
  new(r)

  asgn(r[], a)
  doAssert r[] == a

  asgnViaPtr(r[], b)
  doAssert r[] == b

proc testLvalueConv[T](a, b: T) =
  # lvalue conversion as the argument expression
  type DType = distinct T

  var val: DType
  asgn(T(val), a)
  doAssert T(val) == a

  asgnViaPtr(T(val), b)
  doAssert T(val) == b

proc testCall[T](a, b: T) =
  # test a call returning a mutable view as the argument
  proc mget(x: var T): var T =
    x

  var x: T
  asgn(mget(x), a)
  doAssert x == a

  asgnViaPtr(mget(x), b)
  doAssert x == b

proc testStmtListExpr[T](a, b: T) =
  # statement list expression as the argument expression
  var
    x: T
    y = 0

  asgn((y = 1; x), a)
  doAssert x == a

  asgnViaPtr((y = 1; x), b)
  doAssert x == b

  # also test `y` for good measure
  doAssert y == 1

proc testNestedElement[T](a, b: T) =
  # passing an element inside an array inside an array to a ``var`` parameter
  var arr: array[2, array[2, T]]

  asgn(arr[1][1], a)
  doAssert arr[1][1] == a

  asgnViaPtr(arr[1][1], b)
  doAssert arr[1][1] == b


proc testVarParamLvalueConv[T](a, b: T) =
  # pass an lvalue converted ``var`` parameter to a ``var`` parameter
  proc inner[T, U](x: var T, a, b: U) =
    asgn(U(x), a)
    doAssert U(x) == a

    asgnViaPtr(U(x), b)
    doAssert U(x) == b

  type DType = distinct T

  var x: DType
  inner(x, a, b)
  doAssert T(x) == b

proc testDistinctArray[T, U](a, b: U) =
  # test for the case where both the array and the result of accessing it
  # are put through an lvalue conversion
  type
    ArrT = array[1, T]
    Arr = distinct ArrT

  var arr: Arr

  asgn(U((arr.ArrT)[0]), a)
  doAssert U((arr.ArrT)[0]) == a

  asgnViaPtr(U((arr.ArrT)[0]), b)
  doAssert U((arr.ArrT)[0]) == b

proc test[T](a, b: T) =
  type DType {.used.} = distinct T

  testObjField(a, b)
  testTupleField(a, b)
  testSeq(a, b)
  disableIf defined(js) and T is ref:
    # XXX: ``ref ref T`` leads to invalid code being generated with the JS
    #      back-end
    testRefDeref(a, b)

  testCall(a, b)
  testLvalueConv(a, b)
  testStmtListExpr(a, b)

  # more complex test cases:
  disableIf defined(vm) and (T is Ordinal or T is float or T is ref):
    testNestedElement(a, b)
    testVarParamLvalueConv(a, b)
    testDistinctArray[DType, T](a, b)

type
  Object = object
    val: int
  ObjectRef = ref Object

proc prc1() {.nimcall.} = discard
proc prc2() {.nimcall.} = discard

proc makeClosure(x: int): auto =
  result = proc () = discard x

# it's important to use values that are not equal to the zero representation of
# the type

test('a', 'b')
test(1, 2)
test(1.0, 2.0)
test[range[0..3]](1, 2)
test("a", "b")
test(cstring"a", cstring"b")
test([2, 3], [4, 5])
test(@[2, 3], @[4, 5])
test((2, 3), (4, 5))
test(new int, new int)
test(Object(val: 1), Object(val: 2))
test(ObjectRef(val: 1), ObjectRef(val: 2))

test({false}, {true})             # small set
test[set[uint8]]({1'u8}, {2'u8})  # larger set (32 byte)

test(prc1, prc2)                     # normal procedure
test(makeClosure(1), makeClosure(2)) # closure