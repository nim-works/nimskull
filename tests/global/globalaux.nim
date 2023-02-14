type
  TObj*[T] = object
    val*: T

var
  totalGlobals* = 0

proc makeObj[T](x: T): TObj[T] =
  totalGlobals += 1
  result.val = x

proc globalInstance*[T]: var TObj[T] =
  var g {.global.} = when T is int: makeObj(10) else: makeObj("hello")
  result = g

proc testInline*(cmp: int) {.inline.} =
  # initialization of the global needs to happen at the start of ``globalaux``
  var v {.global.} = 1
  doAssert v == cmp
  # modify the global. The other module from which ``testInline`` is called
  # must be able to observe this modification:
  v = 2

testInline(1) # change the `v` to '2'