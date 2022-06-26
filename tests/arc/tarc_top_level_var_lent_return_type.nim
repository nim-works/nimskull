discard """
  cmd: "nim c --gc:arc $file"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/18971
    Compiler terminated with IndexDefect if --gc:arc or --gc:orc given,
    when proc return a global variable with lent or var type
  . Moving the lines into a proc, no IndexDefect will be raised.
   '''
"""

type MyObj = ref object

var o = MyObj()
proc x: var MyObj = o

var o2 = x()