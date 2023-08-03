discard """
  targets: "c js vm"
  description: "Various tests for closures and closure iterators"
"""

import std/[sequtils, sugar, tables]
import mutils



block tclosure:
  proc map(n: var openarray[int], fn: proc (x: int): int {.closure}) =
    for i in 0..n.len-1: n[i] = fn(n[i])

  proc each(n: openarray[int], fn: proc(x: int) {.closure.}) =
    for i in 0..n.len-1:
      fn(n[i])

  var myData: array[0..4, int] = [0, 1, 2, 3, 4]

  proc testA() =
    var p = 0
    map(myData, proc (x: int): int =
                  result = x + 1 shl (proc (y: int): int =
                    return y + p
                  )(0)
                  inc(p))

  testA()

  var output: seq[int]

  myData.each do (x: int):
    output.add x

  doAssert output == [1, 3, 6, 11, 20]


block array_of_procs:
  # bug https://github.com/nim-lang/nim/issues/5015

  type Mutator = proc(matched: string): string {.noSideEffect, gcsafe, locks: 0.}

  proc putMutated(
      MutatorCount: static[int],
      mTable: static[array[MutatorCount, Mutator]], input: string) =
    for i in 0..<MutatorCount: doAssert mTable[i](input) == "foo"

  proc mutator0(matched: string): string =
      "foo"

  const
    mTable = [Mutator(mutator0)]

  putMutated(1, mTable, "foo")


block tclosure0:
  when true:
    # test simple closure within dummy 'main':
    proc dummy =
      proc main2(param: int) =
        var fooB = 23
        proc outer(outerParam: string) =
          var outerVar = 88
          doAssert outerParam == "foo"
          doAssert outerVar == 88

          proc inner() =
            block Test:
              doAssert fooB == 23
              doAssert param == 24
              doAssert outerParam == "foo"
              doAssert outerVar == 88

          inner()
        outer("foo")
      main2(24)

    dummy()



  # XXX: raw access not supported by the VM
  when not defined(js) and not defined(vm):
    proc outer2(x:int) : proc(y:int):int =   # curry-ed application
        return proc(y:int):int = x*y

    var fn = outer2(6)  # the closure
    doAssert fn(3) == 18 # it works

    var rawP = fn.rawProc()
    var rawE = fn.rawEnv()

    # A type to cast the function pointer into a nimcall
    type TimesClosure = proc(a: int, x: pointer): int {.nimcall.}

    # Call the function with its closure
    doAssert cast[TimesClosure](rawP)(3, rawE) == 18

  when true:
    proc outer =
      var x, y: int = 99
      proc innerA = doAssert x == 99
      proc innerB =
        doAssert y == 99
        innerA()

      innerA()
      innerB()

    outer()

  when true:
    proc indirectDep =
      var x, y: int = 99
      proc innerA =
        doAssert x == 99
        doAssert y == 99

      proc innerB =
        innerA()

      innerA()
      innerB()

    indirectDep()

  when true:
    proc needlessIndirection =
      var x, y: int = 99
      proc indirection =
        var z = 12
        proc innerA =
          doAssert z == 12
          doAssert x == 99
          doAssert y == 99

        proc innerB =
          innerA()

        innerA()
        innerB()
      indirection()

    needlessIndirection()

# XXX: the VM is currently too slow to execute this test case in a reasonable
#      time
test tclosure3, {c, js}:
  proc main =
    const n = 30
    for iterations in 0..10_000:
      var s: seq[proc(): string {.closure.}] = @[]
      for i in 0 .. n-1:
        (proc () =
          let ii = i
          s.add(proc(): string = return $(ii*ii)))()
      for i in 0 .. n-1:
        let val = s[i]()
        if val != $(i*i): echo "bug  ", val

      # if getOccupiedMem() > 5000_000:
      #   doAssert false, "still a leak!"

  main()

# XXX: fails for the VM target because the ``std/streams`` module (used by
#      ``json``) is not yet fully supported there
when not defined(vm):
  import std/json

test tclosure, {c, js}:
  proc run(json_params: OrderedTable) =
    let json_elems = json_params["files"].elems
    # These fail compilation.
    var files = map(json_elems, proc (x: JsonNode): string = x.str)

  let text = """{"files": ["a", "b", "c"]}"""
  run((text.parseJson).fields)


block inference3304:
  # bug https://github.com/nim-lang/nim/issues/3304
  type
    List[T] = ref object
      val: T

  proc foo[T](l: List[T]): seq[int] =
    @[1,2,3,5].filter(x => x != l.val)

  doAssert foo(List[int](val: 3)) == [1, 2, 5]


block tcodegenerr1923:
  type
    Foo[M] = proc() : M

  proc bar[M](f : Foo[M]) =
    discard f()

  proc baz() : int = 42

  bar(baz)



block doNotation:
  type
    Button = object
    Event = object
      x, y: int

  var events: seq[string]

  proc onClick(x: Button, handler: proc(x: Event)) =
    handler(Event(x: 10, y: 20))

  proc onFocusLost(x: Button, handler: proc()) =
    handler()

  proc onUserEvent(x: Button, eventName: string, handler: proc) =
    events.add("registered " & eventName)

  var b = Button()

  b.onClick do (e: Event):
    events.add("click at " & $e.x & "," & $e.y)

  b.onFocusLost:
    events.add("lost focus 1")

  b.onFocusLost do:
    events.add("lost focus 2")

  b.onUserEvent("UserEvent 1") do:
    discard

  b.onUserEvent "UserEvent 2":
    discard

  b.onUserEvent("UserEvent 3"):
    discard

  b.onUserEvent("UserEvent 4", () => events.add("event 4"))

  doAssert events ==
    ["click at 10,20",
     "lost focus 1",
     "lost focus 2",
     "registered UserEvent 1",
     "registered UserEvent 2",
     "registered UserEvent 3",
     "registered UserEvent 4"]


# knownIssue: unrelated to closure support, the JS code-generator emits the
#             ``int32`` overflow checks for 64-bit integers
test fib50, {c, vm}:
  proc memoize(f: proc (a: int64): int64): proc (a: int64): int64 =
      var previous = initTable[int64, int64]()
      return proc(i: int64): int64 =
          if not previous.hasKey i:
              previous[i] = f(i)
          return previous[i]

  var fib: proc(a: int64): int64

  fib = memoize(proc (i: int64): int64 =
      if i == 0 or i == 1:
          return 1
      return fib(i-1) + fib(i-2)
  )

  doAssert fib(50) == 20365011074



block tflatmap:
  # bug https://github.com/nim-lang/nim/issues/3995
  type
    RNG = tuple[]
    Rand[A] = (RNG) -> (A, RNG)

  proc nextInt(r: RNG): (int, RNG) =
    (1, ())

  proc flatMap[A,B](f: Rand[A], g: A -> Rand[B]): Rand[B] =
    (rng: RNG) => (
      let (a, rng2) = f(rng);
      let g1 = g(a);
      g1(rng2)
    )

  proc map[A,B](s: Rand[A], f: A -> B): Rand[B] =
    let g: A -> Rand[B] = (a: A) => ((rng: RNG) => (f(a), rng))
    flatMap(s, g)

  discard nextInt.map(i => i - i mod 2)


block tforum:
  type
    PAsyncHttpServer = ref object
      value: string
    PFutureBase = ref object
      callback: proc () {.closure.}
      value: string
      failed: bool

  proc accept(server: PAsyncHttpServer): PFutureBase =
    new(result)
    result.callback = proc () =
      discard
    server.value = "hahaha"

  proc processClient(): PFutureBase =
    new(result)

  var output = ""

  proc serve(server: PAsyncHttpServer): PFutureBase =
    iterator serveIter(): PFutureBase {.closure.} =
      output.add server.value
      while true:
        var acceptAddrFut = server.accept()
        yield acceptAddrFut
        var fut = acceptAddrFut.value

        var f = processClient()
        f.callback =
          proc () =
            output.add $f.failed

        yield f
    var x = serveIter
    for i in 0 .. 1:
      result = x()
      result.callback()

  discard serve(PAsyncHttpServer(value: "asdas"))

  doAssert output == "asdasfalse"


block futclosure2138:
  proc any[T](list: varargs[T], pred: (T) -> bool): bool =
    for item in list:
        if pred(item):
            result = true
            break

  proc contains(s: string, words: varargs[string]): bool =
    any(words, (word) => s.contains(word))


block tinterf:
  type
    ITest = tuple[
      setter: proc(v: int) {.closure.},
      getter1: proc(): int {.closure.},
      getter2: proc(): int {.closure.}]

  proc getInterf(): ITest =
    var shared1, shared2: int

    return (setter: proc (x: int) =
              shared1 = x
              shared2 = x + 10,
            getter1: proc (): int = result = shared1,
            getter2: proc (): int = return shared2)

  var i = getInterf()
  i.setter(56)

  doAssert i.getter1() == 56
  doAssert i.getter2() == 66

# TODO: reduce the case further
block tjester:
  type
    Future[T] = ref object
      data: T
      callback: proc () {.closure.}

  var output = ""

  proc cbOuter(response: string) {.discardable.} =
    iterator cbIter(): Future[int] {.closure.} =
      for i in 0..7:
        proc foo(): int =
          iterator fooIter(): Future[int] {.closure.} =
            output.add response & $i
            yield Future[int](data: 17)
          var iterVar = fooIter
          iterVar().data
        yield Future[int](data: foo())

    var iterVar2 = cbIter
    proc cb2() {.closure.} =
      try:
        if not finished(iterVar2):
          let next = iterVar2()
          if next != nil:
            next.callback = cb2
      except:
        echo "WTF"
    cb2()

  cbOuter "baro"

  doAssert output == "baro0"



block tnamedparamanonproc:
  type
    PButton = ref object
    TButtonClicked = proc(button: PButton) {.nimcall.}

  proc newButton(onClick: TButtonClicked) =
    discard

  proc main() =
    newButton(onClick = proc(b: PButton) =
      var requestomat = 12
      )

  main()

block tnoclosure:
  proc pascal(n: int) =
    var row = @[1]
    for r in 1..n:
      row = zip(row & @[0], @[0] & row).mapIt(it[0] + it[1])
    doAssert row == [1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1]
  pascal(10)

# knownIssue: the procedure doesn't have an environment parameter, which
#             confuses ``vmgen``
test non_nested_closure, {c, js}:
  # make sure that a top-level anonymous closure procedure works (for now)
  var cl = proc (): int {.closure.} = 1
  doAssert cl is "closure"
  doAssert cl() == 1

block close_over_compile_time_loc:
  proc p() {.compileTime.} =
    var x = 0
    proc inner(cmp: int) = # `inner` is explicitly not compile-time-only
      inc x
      doAssert x == cmp

    inner(1)
    inner(2)

  static:
    p()

template test(body: untyped) {.dirty.} =
  ## Tests that `body` works when placed in:
  ## - a normal procedure
  ## - a procedure that closes over something
  ## - a closure iterator
  ##
  ## A .dirty template is used in order to interfere with the test as little
  ## as possible. Ideally, the template would be expanded before running the
  ## test file.
  proc outerInternal() =
    body

  outerInternal()

  proc outerInternal2() =
    proc innerInternal() =
      body

    innerInternal()

  outerInternal2()

  iterator iterInternal(): int {.closure.} =
    body

  let it = iterInternal
  discard it()

block closure_in_duplicated_for_loop_body:
  # closing over a local defined in the body of a for-loop that is
  # duplicated works. Neither the closed over local nor the procedure is
  # duplicated (in terms of visible behaviour).
  iterator multiYield(): int {.inline.} =
    yield 1
    yield 2

  test:
    var cl: proc(x: int)

    for it in multiYield():
      var x = it

      if cl != nil:
        # the duplicated bodies share the local; invoking the closure would
        # yield x's value from the previous step otherwise
        cl(it)

      proc inner(expect: int) =
        doAssert x == expect

      inner(it) # directly calling the closure procedure works
      if cl == nil:
        cl = inner

    # invoking the closure still works after the loop is finished:
    cl(2)

block capture_in_inline_iterator:
  # closing over the local of an inline iterator is supported, without having to
  # raise the iterator to a closure iterator.

  iterator iter(): int {.inline.} =
    var x = 1
    proc inner(): int =
      inc x
      result = x

    # call ``inner`` multiple times in order to ensure that the modification
    # of `x` persists
    yield inner()
    yield inner()

  test:
    var items: seq[int]
    for it in iter():
      items.add it

    doAssert items == [2, 3]

block closure_closure_iterator_in_closure_iterator:
  # using a closure closure iterator (a closure iterators that closes over
  # some outer locals) works. Each usage of the closure closure iterator
  # expands to the setup of a new environment.

  proc test() =
    var x = 1

    iterator iter2(): int {.closure.} =
      var y = 1 # local state
      yield x + y
      inc x
      inc y
      yield x + y
      inc x
      inc y
      yield x + y

    iterator iter1(): int {.closure.} =
      let z = iter2 # a standalone environment is allocated here
      yield z()
      yield z()
      yield z()

    # all instances of `iter2` read and write to the same `x`
    let it = iter1
    doAssert it() == 2
    doAssert it() == 4
    doAssert it() == 6

    let it2 = iter2 # here too
    doAssert it2() == 4
    doAssert it2() == 6
    doAssert it2() == 8

    doAssert x == 5

  test()

block use_closure_iterator_via_for_syntax:
  # an inner, non-capturing closure iterator can be used in another inner
  # routine via the for-syntax

  proc outer() =
    iterator iter(): int {.closure.} =
      # a closure iterator that doesn't close over any locals
      var x = 1
      yield x
      inc x
      yield x

    test:
      var compare = 1
      for it in iter():
        doAssert it == compare
        inc compare

  outer()