discard """
matrix: "-d:release"
labels: "codegen mode_release"
description: '''
  . From https://github.com/nim-lang/Nim/issues/5296
    Compiling in release mode prevents valid code execution.
  . This seems related to C compiler optimisation. I'm reasonably
    sure the problem is that the variable isn't declared as volatile.
  . In optimizing mode, compilers often discard the variable and keep
    them register. which can mess up state restoration on longjmp
'''
"""

proc bug() : void =
    var x = 0
    try:
        inc x
        raise new(Exception)
        doAssert false
    except Exception:
        doAssert x == 1

bug()
