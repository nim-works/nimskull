discard """
description: "Tests for the experimental call operator `()` overloading"
"""

import macros

{.experimental: "callOperator".}

# setup - declare up front so we can ensure overloading works

type Foo[T: proc] = object
  callback: T

macro `()`(foo: Foo, args: varargs[untyped]): untyped =
  result = newCall(newDotExpr(foo, ident"callback"))
  for a in args:
    result.add(a)

proc `()`(args: varargs[string]): string =
  result = "("
  for a in args: result.add(a)
  result.add(')')

macro `()`(args: varargs[untyped]): untyped =
  # this should be picked last... based on current rules
  result = newNimNode(nnkTupleConstr)
  for i, a in args.pairs:
    result.add(a.toStrLit)

block macro_over_some_foo:
  var f1Calls = 0
  var f = Foo[proc()](callback: proc() = inc f1Calls)
  doAssert f1Calls == 0
  f()
  doAssert f1Calls == 1

  # what if we change the callable?
  var f2Calls = 0
  f.callback = proc() = inc f2Calls
  doAssert f2Calls == 0
  f()
  doAssert f2Calls == 1
  doAssert f1Calls == 1 # `f1Calls` remains unscathed from our shenanigans

  let g = Foo[proc (x: int): int](callback: proc (x: int): int = x * 2 + 1)
  doAssert g(15) == 31

block we_can_even_use_a_regular_proc:
  # note it still dispatches correctly on params of type string

  let a = "1"
  let b = "2"
  let c = "3"

  doAssert a(b) == "(12)"
  doAssert a.b(c) == `()`(b, a, c)
  doAssert (a.b)(c) == `()`(a.b, c)
  doAssert `()`(a.b, c) == `()`(`()`(b, a), c)

block works_with_untyped_input:
  # previously an error was reported for `doesNotExist` being undeclared
  let something = 10

  doAssert something(doesNotExist) == ("something", "doesNotExist")

block does_not_interfere_with_ambiguous_lookups_requiring_overload_resolution:
  # this was a regression
  proc floop(input: string, len: int): string =
    if input.len == len: # here the `len` proc must go through overload
                         # resolution, instead of the call operator
      "same length"
    else:
      "different lengths"

  doAssert floop("test", 4) == "same length"
  doAssert "test".floop(4) == "same length"