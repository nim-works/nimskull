## Common code for the `access_checks` family of tests

type Object* = object
  a*: int32
  b*: bool
  c*: string
  d*: seq[int]

func asPtr*[A, B](x: var A, t: typedesc[B]): ptr B =
  cast[ptr B](addr x)

proc localPtr*[T](t: typedesc[T]): ptr T =
  ## Returns a pointer to a local variable
  var x: T
  result = addr x

template objectTest*(targetTyp: type, code) =
  static:
    var o = Object(a: 3, b: true, c: "c", d: @[1, 2])
    let p {.inject.} = cast[ptr targetTyp](addr o)
    code