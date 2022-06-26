discard """
  action: run
  cmd: "nim c --gc:arc $file"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/15909
      Compiler crash using if as an expression with a noreturn branch

    . The compiler crashes with SIGSEGV: Illegal storage access.
      (Attempt to read from nil?) when attempting to compile the following
      source with --gc:arc, --gc:orc or --newruntime
'''
"""

proc f1() {.noreturn.} = raise newException(CatchableError, "")

proc f2(y: int): int =
  if y != 0:
    y
  else:
    f1()

doAssert f2(5) == 5
doAssertRaises(CatchableError):
  discard f2(0)