discard """
  errormsg: "a converter takes exactly one argument"
  file: "tconverter_at_most_one_arg.nim"
  line: 7
"""

converter foo(a: int, b: string): float = 1.0

echo foo(1, "two")