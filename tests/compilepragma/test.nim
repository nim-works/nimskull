discard """
  output: '''44'''
  joinable: "false"
  target: c
"""

{.compile: "test.c".}

proc foo(a, b: cint): cint {.importc: "foo", cdecl.}

echo foo(40, 4)
