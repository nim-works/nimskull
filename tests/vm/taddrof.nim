discard """
nimout: '''
true
true
[nil, nil, nil, nil]
[MyObjectRef(123, 321), nil, nil, nil]
['A', '\x00', '\x00', '\x00']
MyObjectRef(123, 321)
(key: 8, val: 0)
'''
output: '''
true
true
[nil, nil, nil, nil]
[MyObjectRef(123, 321), nil, nil, nil]
['A', '\x00', '\x00', '\x00']
MyObjectRef(123, 321)
'''
"""

type
  MyObjectRef = ref object
    a,b: int

  MyContainerObject = ref object
    member: MyObjectRef

  MySuperContainerObject = ref object
    member: MyContainerObject
    arr: array[4, MyObjectRef]

  MyOtherObject = ref object
    case kind: bool
    of true:
      member: MyObjectRef
    else:
      discard

proc `$`(arg: MyObjectRef): string =
  result = "MyObjectRef("
  result.addInt arg.a
  result.add ", "
  result.addInt arg.b
  result.add ")"

proc foobar(dst: var MyObjectRef) =
  dst = new(MyObjectRef)
  dst.a = 123
  dst.b = 321

proc changeChar(c: var char) =
  c = 'A'

template assertStrEq(s, expect: string) =
  {.line.}:
    doAssert s == expect, s

proc test() =
  # when it comes from a var, it works
  var y: MyObjectRef
  foobar(y)
  echo y != nil
  # when it comes from a member, it fails on VM
  var x = new(MyContainerObject)
  foobar(x.member)
  echo x.member != nil

  # when it comes from an array, it fails on VM
  var arr: array[4, MyObjectRef]
  echo arr
  foobar(arr[0])
  echo arr

  var arr2: array[4, char]
  changeChar(arr2[0])
  echo arr2


  var z = MyOtherObject(kind: true)
  foobar(z.member)
  echo z.member


  var sc = new(MySuperContainerObject)
  sc.member = new(MyContainerObject)
  foobar(sc.member.member)
  doAssert sc.member.member.a == 123
  doAssert sc.member.member.b == 321
  foobar(sc.arr[1])
  doAssert sc.arr[1].a == 123
  doAssert sc.arr[1].b == 321

  var str = "---"
  changeChar(str[1])
  assertStrEq str, "-A-"

test()
static:
  test()

type T = object
  f: seq[tuple[key, val: int]]

proc foo(s: var seq[tuple[key, val: int]]; i: int) =
  s[i].key = 4*i
  # r4 = addr(s[i])
  # r4[0] = 4*i

proc bar() =
  var s: T
  s.f = newSeq[tuple[key, val: int]](3)
  foo(s.f, 2)
  echo s.f[2]

static:
  bar()


block addr_tuple_array:
  proc t() =
    var a = [(1, 2)]
    # Test nkBracketExpr(nkBracketExpr()) where the outer
    # subscript operator is for the tuple
    let p = addr(a[0][0])
    doAssert p[] == 1

  static:
    t()

block addr_array_array:
  type A = object
    x: int

  proc t() =
    var a1: array[1, int]
    var a = [A(x: 10)]
    let p = unsafeAddr(a[a1[0]])
    doAssert p.x == 10

  static:
    t()

block addr_dot_dot:
  # Make sure that `addr(a.b.c)` works when c is:
  # * an array field subscript expression
  # * an int field
  # * a object field
  # * a string field subscript expression
  type
    C = object
      x: int
    B = object
      a: array[2, int]
      b: int
      c: C
      d: string
      f: seq[string]
    A = object
      b: B

  proc t(a: A) =
    let p1 = unsafeAddr(a.b.a[0])
    doAssert p1[] == 1
    let p2 = unsafeAddr(a.b.b)
    doAssert p2[] == 3
    let p3 = unsafeAddr(a.b.c)
    doAssert p3.x == 4
    let p4 = unsafeAddr(a.b.d[2])
    # doesn't work right now, due to char being treated as having
    # a size of 8-byte
    #doAssert p4[] == 's'


  static:
    var a = A(b: B(a: [1, 2], b: 3, c: C(x: 4), d: "test", f: @["str"]))
    t(a)
