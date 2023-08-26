discard """
  errormsg: "a tuple value of the form '(string, int, int)' is expected, but got: '(\"\",)'"
  line: 6
"""

{.line: ("",).}:
  discard