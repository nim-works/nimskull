discard """
  errormsg: "type mismatch: got <typedesc[int]>"
  line: 12
"""

# bug https://github.com/nim-lang/nim/issues/3079
# bug https://github.com/nim-lang/nim/issues/1146

proc p[T](x: T) =
  discard

p(int)
