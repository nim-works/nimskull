discard """
  errormsg: "type mismatch: got <string>"
  nimout: "twrapconcept.nim(10, 13) Foo: concept predicate failed"
  knownIssue: "Requires Report/SemReport removal, see: https://github.com/nim-works/nimskull/issues/443"
"""

# https://github.com/nim-lang/Nim/issues/5127

type
  Foo = concept foo
    foo.get is int

  FooWrap[F: Foo] = object
    foo: F

proc get(x: int): int = x

proc wrap[F: Foo](foo: F): FooWrap[F] = FooWrap[F](foo: foo)

let x = wrap(12)
let y = wrap "string"

