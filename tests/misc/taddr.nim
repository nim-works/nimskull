discard """
  targets: "c js vm"
  matrix: "; -d:release"
"""

template disableVm(code: untyped) =
  ## Only executes `code` if not running in the VM
  when nimvm:
    discard
  else:
    when not defined(vm):
      block:
        code

type T = object
  x: int
  s: string

var obj: T
var fieldAddr = addr(obj.x)
var objAddr = addr(obj)

# Integer tests
var field = fieldAddr[]
doAssert field == 0

var objDeref = objAddr[]
doAssert objDeref.x == 0

# Change value
obj.x = 42

doAssert field == 0
doAssert objDeref.x == 0

field = fieldAddr[]
objDeref = objAddr[]

doAssert field == 42
doAssert objDeref.x == 42

# String tests
obj.s = "lorem ipsum dolor sit amet"
prepareMutation(obj.s)
var indexAddr = addr(obj.s[2])

doAssert indexAddr[] == 'r'

indexAddr[] = 'd'

doAssert indexAddr[] == 'd'

doAssert obj.s == "lodem ipsum dolor sit amet"

# Bug #2148
var x: array[2, int]
var y = addr x[1]

y[] = 12
doAssert(x[1] == 12)

type
  Foo = object
    bar: int

var foo: array[2, Foo]
var z = addr foo[1]

z[].bar = 12345
doAssert(foo[1].bar == 12345)

var t : tuple[a, b: int]
var pt = addr t[1]
pt[] = 123
doAssert(t.b == 123)

#block: # Test "untyped" pointer.
proc testPtr(p: pointer, a: int) =
  doAssert(a == 5)
  (cast[ptr int](p))[] = 124
var i = 123
testPtr(addr i, 5)
doAssert(i == 124)

var someGlobal = 5
proc getSomeGlobalPtr(): ptr int = addr someGlobal
let someGlobalPtr = getSomeGlobalPtr()
doAssert(someGlobalPtr[] == 5)
someGlobalPtr[] = 10
doAssert(someGlobal == 10)

block:
  # bug #14576
  # lots of these used to give: Error: internal error: genAddr: 2
  proc byLent[T](a: T): lent T = a
  proc byPtr[T](a: T): ptr T = a.addr

  block:
    let a = (10,11)
    let (x,y) = byLent(a)
    doAssert (x,y) == a

  # knownIssue: borrowing from parameters that are of primitive type doesn't
  #             work yet
  disableVm: # (with -d:release) bug #14578
    let a = 10
    doAssert byLent(a) == 10
    let a2 = byLent(a)
    doAssert a2 == 10

  block:
    let a = [11,12]
    doAssert byLent(a) == [11,12] # bug #15958
    let a2 = (11,)
    doAssert byLent(a2) == (11,)

  block:
    proc byLent2[T](a: seq[T]): lent T = a[1]
    var a = @[20,21,22]
    doAssert byLent2(a) == 21

  block: # sanity checks
    proc bar[T](a: var T): var T = a
    var a = (10, 11)
    let (k,v) = bar(a)
    doAssert (k, v) == a
    doAssert k == 10
    bar(a)[0]+=100
    doAssert a == (110, 11)
    var a2 = 12
    doAssert bar(a2) == a2
    bar(a2).inc
    doAssert a2 == 13

  block: # pending bug #15959
    when false:
      proc byLent2[T](a: T): lent type(a[0]) = a[0]

proc test14420() = # bug #14420
  # s/proc/template/ would hit bug #16005
  block:
    type Foo = object
      x: float

    proc fn(a: var Foo): var float =
      ## WAS: discard <- turn this into a comment (or a `discard`) and error disappears
      # result = a.x # this works
      a.x #  WAS: Error: limited VM support for 'addr'

    proc fn2(a: var Foo): var float =
      result = a.x # this works
      a.x #  WAS: Error: limited VM support for 'addr'

    var a = Foo()
    discard fn(a)
    discard fn2(a)

  # knownIssue: both the JS and VM target don't support borrowing from the
  #             first parameter when it's of primitive type
  when not defined(js) and not defined(vm):
    proc byLent2[T](a: T): lent T =
      runnableExamples: discard
      a
    proc byLent3[T](a: T): lent T =
      runnableExamples: discard
      result = a
    var a = 10
    let x3 = byLent3(a) # works
    let x2 = byLent2(a) # WAS: Error: internal error: genAddr: nkStmtListExpr

  block:
    type MyOption[T] = object
      case has: bool
      of true:
        value: T
      of false:
        discard
    func some[T](val: T): MyOption[T] =
      result = MyOption[T](has: true, value: val)
    func get[T](opt: MyOption[T]): lent T =
      doAssert opt.has
      # result = opt.value # this was ok
      opt.value # this had the bug
    let x = some(10)
    doAssert x.get() == 10

template test14339() = # bug #14339
  block:
    type
      Node = ref object
        val: int
    proc bar(c: Node): var int =
      var n = c # was: Error: limited VM support for 'addr'
      c.val
    var a = Node()
    discard a.bar()
  block:
    type
      Node = ref object
        val: int
    proc bar(c: Node): var int =
      var n = c
      doAssert n.val == n[].val
      n.val
    var a = Node(val: 3)
    a.bar() = 5
    when nimvm:
      doAssert a.val == 5
    else:
      when not defined(js): # pending bug #16003
        doAssert a.val == 5

template testStatic15464() = # bug #15464
  proc access(s: var seq[char], i: int): var char = s[i]
  proc access(s: var string, i: int): var char = s[i]
  static:
    var s = @['a', 'b', 'c']
    access(s, 2) = 'C'
    doAssert access(s, 2) == 'C'
  static:
    var s = "abc"
    access(s, 2) = 'C'
    doAssert access(s, 2) == 'C'

proc test15464() = # bug #15464 (v2)
  proc access(s: var seq[char], i: int): var char = s[i]
  proc access(s: var string, i: int): var char = s[i]
  block:
    var s = @['a', 'b', 'c']
    access(s, 2) = 'C'
    doAssert access(s, 2) == 'C'
  block:
    var s = "abc"
    access(s, 2) = 'C'
    doAssert access(s, 2) == 'C'

block: # bug #15939
  block:
    const foo = "foo"
    proc proc1(s: var string) =
      if s[^1] notin {'a'..'z'}:
        s = ""
    proc proc2(f: string): string =
      result = f
      proc1(result)
    const bar = proc2(foo)
    doAssert bar == "foo"

proc test15939() = # bug #15939 (v2)
  template fn(a) =
    when typeof(a) is string:
      prepareMutation(a)

    let pa = a[0].addr
    doAssert pa != nil
    doAssert pa[] == 'a'
    pa[] = 'x'
    doAssert pa[] == 'x'
    doAssert a == "xbc"
    when not defined js: # otherwise overflows
      let pa2 = cast[ptr char](cast[int](pa) + 1)
      doAssert pa2[] == 'b'
      pa2[] = 'B'
      doAssert a == "xBc"

  # mystring[ind].addr
  var a = "abc"
  fn(a)

  # mycstring[ind].addr
  template cstringTest =
    var a2 = "abc"
    prepareMutation(a2)
    var b2 = a2.cstring
    fn(b2)
  when nimvm: cstringTest()
  else: # can't take address of cstring element in js
    when not defined(js): cstringTest()

block immutable_parameter:
  proc testArg[T](x: ptr T, val: T) =
    doAssert x[] == val

  proc test[T](x: T) =
    # test initializing assignment:
    var p = unsafeAddr x
    doAssert p[] == x

    # test a normal assignment:
    p = unsafeAddr x
    doAssert p[] == x

    # test passing to a ptr parameter:
    testArg(unsafeAddr x, x)

  type Obj = object
    val: int

  when not defined(js):
    # XXX: invalid code is generated with the JS back-end
    test(1)                 # int
    test(1.0)               # float
    test(@[1, 2])           # seq
    test("str")             # string
    test(cstring"str")      # cstring
    test[range[0..1]](1)    # range
    test(new int)           # ref to int
    test((ref Obj)(val: 1)) # ref to object

    test(proc() {.nimcall.} = discard) # normal procedure
    test(proc() {.closure.} = discard) # closure

  test([1, 2])      # array
  test({false})     # set
  test(Obj(val: 1)) # object

block immutable_for_var_addr:
  template test() =
    for val in [1]:
      let p = unsafeAddr val
      doAssert p[] == val

  # test with a global for-var
  test()

  # test with a local for-var
  proc prc() =
    test()
  prc()

template main =
  test14339()
  test15464()
  test15939()

testStatic15464()
static: main()
main()
