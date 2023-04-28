discard """
  output: '''@[0]
@[1]
@[2]
@[3]'''
  joinable: false
"""

# bug #6434

type
  Foo* = object
    boo: int

var sink_counter = 0
var assign_counter = 0

proc `=sink`(dest: var Foo, src: Foo) =
  sink_counter.inc

proc `=`(dest: var Foo, src: Foo) =
  assign_counter.inc

proc createFoo(): Foo = Foo(boo: 0)

proc test(): auto =
  var a, b = createFoo()
  return (a, b, Foo(boo: 5))

var (ag, bg, _) = test()

doAssert assign_counter == 0
doAssert sink_counter == 0

# bug #11510
proc main =
  for i in 0 ..< 4:
    var buffer: seq[int] # = @[] # uncomment to make it work
    # var buffer: string # also this is broken
    buffer.add i
    echo buffer

main()

block move_from_temporary_inner:
  # regression test: partially moving from a materialized-into-temporary
  # object returned by a call led to the moved-from location not being reset,
  # causing a double free. The issue only ocurred when the path expression
  # doesn't name a fixed location (e.g. an array access with a non-constant
  # index value), the moved-from location is of numeric type, or both
  type
    Inner = distinct int
    Outer = array[1, Inner]

  var numDestroyed = 0

  proc `=destroy`(x: var Inner) =
    if ord(x) != 0:
      inc numDestroyed
      # don't manually set `x` to 0 here -- the injected ``wasMoved`` needs to
      # take care of that

  proc produce(): Outer = [Inner(1)]

  proc test(i: int) =
    # use a parameter for the index in order to prevent the optimizer from
    # inlining the value
    var x = produce()[i]
    doAssert ord(x) == 1

  test(0)

  doAssert numDestroyed == 1