
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

  # this must not raise a ``RangeDefect``. Note that '-1' has to be provided
  # by a temporary because ``semfold`` would report a ``RangeDefect``
  # otherwise
  let x = -1
  doAssert x notin {1, 2}

block set_offset:
  type Enum = enum
    # for the test, it's important that the values are outside the uint8
    # range
    a = 1024, b = 1025

  doAssert sizeof(set[Enum]) == 1

  var s: set[Enum]
  doAssert a notin s

  s.incl a
  doAssert a in s
  doAssert s == {a}

  s.incl b
  s.excl a
  doAssert a notin s
  doAssert s == {b}