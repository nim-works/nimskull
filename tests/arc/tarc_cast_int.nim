discard """
  matrix: "--gc:arc"
  errormsg: "expression cannot be cast to int"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/16558
    . Regression with cast[int](any value) when using --gc:arc
      (invalid C code generated).
    . This cannot work with gc:arc since strings are a (len, pointer) tuple there.
    . It is discouraged to rely on implementation details, but it
      should not crash. tyString and tySequence are value types for ARC ORC
      instead of pointer types. If the internal details change again
      in the future, the test case should catch it.
    . Similar issues are
      https://github.com/nim-lang/Nim/issues/13154 and
      https://github.com/nim-lang/Nim/issues/13864
  '''
"""

block:
  var value = "hi there"
  var keepInt: int
  keepInt = cast[int](value)

