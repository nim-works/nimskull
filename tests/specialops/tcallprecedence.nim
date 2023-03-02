import macros

{.experimental: "dotOperators".}
{.experimental: "callOperator".}

block:
  template `.()`(foo: int, args: varargs[untyped]): untyped {.used.} =
    ".()"

  template `()`(foo: int, args: varargs[untyped]): untyped =
    "()"

  let a = (b: 1)
  let c = 3

  doAssert a.b(c) == "()"
  doAssert not compiles(a(c))
  doAssert (a.b)(c) == "()"

macro `()`(args: varargs[typed]): untyped =
  result = newLit("() " & args.treeRepr)

macro `.`(args: varargs[typed]): untyped =
  result = newLit(". " & args.treeRepr)

block dot_operators_precede_call_operator:
  let a = 1
  let b = 2
  doAssert a.b == `.`(a, b)

block not_confused_by_overloads:
  let a = 1
  proc b(): int {.used.} = 2
  doAssert a.b == `.`(a, b)

block existing_proc_vs_special_ops_fallback:
  let a = 1
  proc b(x: int): int = x + 1
  let c = 3

  doAssert a.b(c) == `.`(a, b, c)
  doAssert a(b) == `()`(a, b)
  doAssert (a.b)(c) == `()`(a.b, c)
  doAssert a.b == b(a)
