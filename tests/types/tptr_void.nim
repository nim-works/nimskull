discard """
  errormsg: "type \'ptr void\' is not allowed"
  line: 13
  description: '''
    . From https://github.com/nim-lang/Nim/issues/6456
      Compiler crashes on ptr void
    . The Nim compiler seems to crash when objects have a ptr void member
      and neither give an explicit error message nor a line number
    . It shouldn't crash, but you can't do "ptr void"
      use "pointer" instead
  '''
"""
proc foo(x: ptr void) =
  discard

