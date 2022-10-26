discard """
  errormsg: "a converter takes exactly one argument"
  file: "tconverter_at_least_one_arg.nim"
  line: 7
"""

converter foo(): float = 1.0

echo foo()