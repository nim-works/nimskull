discard """
  description: '''
    Regression test for a C code generator issues where closure constructions
    resulted in C code that some strict compilers rejected
  '''
  targets: "c"
  joinable: false
"""
# the test uses a custom configuration file, so don't join it

type
  NimCallProc = proc () {.nimcall.}
  Closure = proc() {.closure.}

var p1: NimCallProc
var p2: Closure = p1
# ^^ this was missing an explicit function pointer cast