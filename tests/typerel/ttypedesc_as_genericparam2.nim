discard """
  errormsg: "type mismatch: got <void>"
  line: 9
"""

# bug https://github.com/nim-lang/nim/issues/2879

var s: seq[int]
echo repr(s.new_seq(3))
