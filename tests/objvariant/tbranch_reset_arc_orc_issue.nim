discard """
  action: run
  description: "Test for "
  targets: "c cpp"
  matrix: "--gc:arc; --gc:orc"

  knownIssue: ""
"""

# ARC/ORC doesn't inject branch reset logic for discriminators that are
# part of objects which don't have an auto-generated destructor

block:
  type A = object # `A` has no auto-generated destructor
    case k: bool
    of false:
      a: int
    of true:
      b: int

  var a = A(k: false, a: 1)
  a.k = true # change branch
  doAssert a.b == 0 # `a.a` was not reset and since it shares it's location
                    # with `a.b`, `a.b` is 1
