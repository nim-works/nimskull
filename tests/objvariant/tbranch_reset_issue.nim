discard """
  description: "Padding bytes are not reset when switching branches"
  targets: "c cpp"
  matrix: "--gc:refc; --gc:arc; --gc:orc"

  outputsub: '''[AssertionDefect]'''
  exitcode: 1
"""

block:
  type T = object
    case k: bool
    of false:
      x: uint32
    of true:
      y: uint64

  var t = T(k: false)
  cast[ptr uint64](addr(t.x))[] = high(uint64) # Write outside the
                                               # locations storage
  t.k = true
  doAssert t.y == 0 # fails