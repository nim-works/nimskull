discard """
labels: "codegen error_runtime exception"
description: '''
  . From https://github.com/nim-lang/Nim/issues/9286
    Exception doesn't clear result value
  . The second time should return none, but isSome(opt) is always equals to true
  . Wrong heuristic in codegen. A bare return may trigger the insertion
    of a genericReset
  . This was a regression, introduced in 0.19, 0.18 doesn't have this problem.
'''
"""

import options
type Foo  = ref object
  i:      int

proc next(foo: Foo): Option[Foo] =
  try:    doAssert(foo.i == 0)
  except: return      # 2º: none
  return some(foo)    # 1º: some

proc test =
  let foo = Foo()
  var opt = next(foo) # 1º Some
  while isSome(opt) and foo.i < 10:
    inc(foo.i)
    opt = next(foo)   # 2º None
  doAssert foo.i == 1, $foo.i

test()