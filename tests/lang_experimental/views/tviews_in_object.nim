discard """
  targets: "c js vm"
  matrix: "--experimental:views"
  description: '''
    Tests for direct single-location views used as the type of
    object fields. Only read access is tested here.
  '''
"""

# note: all the tests here are run in the context of procedures; tests for
# views in the context of top-level code should go elsewhere

template testCase(typ: typed, initial, codeA: untyped) {.dirty.} =
  block:
    type
      ObjImmutable = object
        field: lent typ
      ObjMutable = object
        field: var typ

    proc testA() =
      let
        local: typ = initial
        o = ObjImmutable(field: local)
      codeA

    testA()

    proc testB() =
      var
        local: typ = initial
        o = ObjMutable(field: local)
      codeA

    testB()

# test: single primitive location
testCase(int, 1):
  doAssert o.field == 1

# test: single array location
testCase(array[1, int], [1]):
  doAssert o.field[0] == 1 # subscript access works
  doAssert o.field == [1]

# test: single string location
testCase(string, "abc"):
  doAssert o.field[1] == 'b' # subscript access works
  doAssert o.field == "abc"

# test: single seq location
testCase(seq[int], @[1]):
  doAssert o.field[0] == 1 # subscript access works
  doAssert o.field == [1]

# XXX: internal compiler error in ``cgirgen``
when false:
  # test: single tuple location
  testCase(tuple[x: int], (1,)):
    doAssert o.field[0] == 1 # subscript access works
    doAssert o.field.x == 1  # dot access works
    doAssert o.field == (1,)

# test: single object location
type Object = object
  value: int
testCase(Object, Object(value: 1)):
  doAssert o.field.value == 1
  doAssert o.field == Object(value: 1)