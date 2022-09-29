
template tmp(x: untyped): untyped =
  # prevent constant folding by introducing a temporary
  let v = x
  v

type Elem = enum
    a, b, c, d, e

block setops:
  doAssert tmp(255'u8) in {0'u8, 255'u8}
  doAssert tmp(1'u8) notin {0'u8}

  doAssert tmp({a}) < {a, b}
  doAssert not(tmp({a}) < {a})
  doAssert not(tmp({a, b}) < {a})

  doAssert (set[Elem])({}) <= tmp({a})
  doAssert tmp({a}) <= {a, b}
  doAssert tmp({a}) <= {a}