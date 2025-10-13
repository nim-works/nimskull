discard """
  description: '''
    Regression test for a bug with querying the calling convention of a MIR
    type. Derived from https://github.com/nim-works/nimskull/issues/1394.
  '''
"""

# calling convention doesn't matter, as long as the proc type is not a closure
# type
var p: proc(x: int) {.nimcall, varargs.}
