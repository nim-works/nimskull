discard """
  targets: "c js vm"
  output: ""
"""

import std/sequtils
import mutils

block tissue600:
  for i in 1..1:
    var reported = false
    proc report() =
      reported = true


block tissue1502def:
  let xs: seq[tuple[key: string, val: seq[string]]] = @[("foo", @["bar"])]

  let maps = xs.map(
    proc(x: auto): tuple[typ: string, maps: seq[string]] =
      (x.key, x.val.map(proc(x: string): string = x)))



block tissue1642:
  var i = 0
  proc p() = inc(i)



block tissue1846:
  type
    TBinOp[T] = proc (x,y: T): bool
    THeap[T] = object
      cmp: TBinOp[T]

  proc less[T](x,y: T): bool =
    x < y

  proc initHeap[T](cmp: TBinOp[T]): THeap[T] =
    result.cmp = cmp

  var h = initHeap[int](less[int])
  doAssert h.cmp(2,3) == true



block tissue1911:
  proc foo(x: int) : auto =

    proc helper() : int = x
    proc bar() : int = helper()
    proc baz() : int = helper()

    return (bar, baz)

# bug https://github.com/nim-lang/nim/issues/11523
proc foo(): proc =
  let a = 999
  return proc(): (int, int) =
    return (a, 0)

doAssert foo()() == (999, 0)

# knownIssue: `i` is treated as a global defined at the procedure level, which
#             are not properly supported by the JS backend yet
test tissue7104, {c, vm}:
  var output: seq[int]

  proc sp(cb: proc())=
      cb()

  sp:
      var i = 0
      output.add(i)
      sp():
          inc i
          output.add(i)
          sp do:
              inc i
              output.add(i)

  doAssert output == [0, 1, 2]

block lifted_var_in_loop_body:
  # bug https://github.com/nim-lang/nim/issues/5519

  iterator iter(): int =
    # it's important that the iterator has at least two yield statements,
    # so that new variables are introduced for each inlined loop body
    yield 1
    yield 1

  proc tup(): (int, int) {.noinline.} =
    ## used to prevent the optimizer from interfering with the test
    result = (1, 2)

  proc outer() =
    for _ in iter():
      # a var tuple where at least one symbol was lifted into the environment
      # casued the compiler to crash, due to ``transf`` not considering the
      # possibility of a ``nkVarTuple`` containing lifted locals
      var (a, b) = tup()

      proc cap() {.closure.} =
        # capture `a`
        doAssert a == 1

      cap()

block:
  # a regression test against closure inference happening too early (i.e.
  # during early overload resolution)
  proc val(x: int): int = result

  proc test() =
    var val = 1 # it's important that the name matches that of the procedure
    proc inner(i: int) =
      # because `val` is ambiguous, the ``val(i)`` expression is analysed by
      # ``semIndirectOp``, which resolved `val` to the starting symbol to use
      # for overload resolution via ``semExpr``. The closest symbol named
      # `val` (that of the local in this case) is chosen, and due to capture
      # analysis previously being run when an identifier was turned into a
      # symbol, this meant that `inner` was erroneously inferred as capturing
      # something
      discard val(i)

    doAssert typeof(inner) isnot "closure"
    inner(1)

  test()

block:
  # a regression test against closure inference modifying symbol state when
  # being run inside a ``compiles`` context
  proc test() =
    var val = 0
    proc inner() =
      doAssert compiles(val)

    doAssert typeof(inner) isnot "closure"
    inner()

  test()