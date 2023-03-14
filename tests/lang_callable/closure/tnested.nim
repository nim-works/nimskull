discard """
  targets: "c js vm"
  description: "Tests for nested closures and closure iterators"
"""

import mutils

block tnestedclosure:
  proc main(param: int) =
    var foo = 23
    proc outer(outerParam: string) =
      var outerVar = 88
      doAssert outerParam == "foo"
      doAssert outerVar == 88
      proc inner() =
        block Test:
          doAssert foo == 23
          doAssert param == 24
          doAssert outerParam == "foo"
          doAssert outerVar == 88
      inner()
    outer("foo")

  main(24)

  # Jester + async triggered this bug:
  proc cbOuter() =
    var response = "hohoho"
    block:
      proc cbIter() =
        block:
          proc fooIter() =
            doAssert response == "hohoho"
          fooIter()
      cbIter()
  cbOuter()

block tnestedproc:
  proc p(x, y: int): int =
    result = x + y

  doAssert p((proc (): int =
            var x = 7
            return x)(),
        (proc (): int = return 4)()) == 11

# knownIssue: closure iterators are not supported with the VM and JS target yet
test deeplynested, {c}:
  # bug https://github.com/nim-lang/nim/issues/4070
  var output: seq[int]

  proc id(f: (proc())): auto =
    return f

  proc foo(myinteger: int): (iterator(): int) =
    return iterator(): int {.closure.} =
            proc bar() =
              proc kk() =
                output.add myinteger
              kk()
            id(bar)()

  discard foo(108)()

  doAssert output == [108]


block tclosure2:
  block:
    var output: seq[int]
    proc ax =
      for xxxx in 0..9:
        var i = 0
        proc bx =
          if i > 10:
            output.add xxxx
            return
          i += 1
          #for j in 0 .. 0: echo i
          bx()

        bx()
        output.add i

    ax()

    doAssert output ==
      [0, 11, 1, 11, 2, 11, 3, 11, 4, 11, 5, 11, 6, 11, 7, 11, 8, 11, 9, 11]

  when true:
    proc accumulator(start: int): (proc(): int {.closure.}) =
      var x = start-1
      #let dummy = proc =
      #  discard start

      result = proc (): int =
        #var x = 9
        for i in 0 .. 0: x = x + 1

        return x

    var a = accumulator(3)
    let b = accumulator(4)
    doAssert a() + b() + a() == 11

  block:
    var counter = 0

    proc outer =

      proc py() =
        # no closure here:
        for i in 0..3: inc counter

      py()

    outer()

    doAssert counter == 4


  when true:
    var output: seq[string]
    proc outer2 =
      var errorValue = 3
      proc fac[T](n: T): T =
        if n < 0: result = errorValue
        elif n <= 1: result = 1
        else: result = n * fac(n-1)

      proc px() {.closure.} =
        output.add "px"

      proc py() {.closure.} =
        output.add "py"

      let
        mapping = {
          "abc": px,
          "xyz": py
        }
      mapping[0][1]()

      output.add $fac(3)


    outer2()

    doAssert output == ["px", "6"]

# bug https://github.com/nim-lang/nim/issues/5688

import std/typetraits

proc myDiscard[T](a: T) = discard

proc foo() =
  let a = 5
  let f = (proc() =
             myDiscard (proc() = (echo a; doAssert false))
          )
  doAssert name(typeof(f)) == "proc (){.closure, gcsafe, locks: 0.}"

foo()

