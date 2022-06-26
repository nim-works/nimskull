discard """
  nimout: '''@["1", "2", "3"]'''
  description: '''
    . From https://github.com/nim-lang/Nim/issues/12558
      mapIt from sequtils not working in {.push compile_time.} context
    . It works with macros, which are also compile-time only
    . map_it does work in compileTime procs, it is the {.push compile_time.}
      that causes the problem.
  '''
"""

import sequtils

{.push compile_time.}

proc foo =
  echo map_it([1, 2, 3], $it)

{.pop.}

static:
  foo()


