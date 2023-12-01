discard """
description: '''
Mutating variable while passing it to the procedure
arguments works differently on different backends.
'''
knownIssue.c js: "https://github.com/nim-lang/Nim/issues/18099"
"""


proc impl(a, b, c: int): (int, int, int) = (a, b, c)

block use_temporary:
  var i = 0
  let a = i
  let b = (inc i; i)
  let c = (inc i; i)
  doAssert impl(a, b, c) == (0, 1, 2)




block fail_to_mutate_argument:
  ## Mutating argument in-place reuses final value for all
  ## arguments. Expected `(0, 1, 2)`, but right now `impl` returns
  ## `(2, 2, 2)`
  ##
  ## Works correctly with ARC and ORC, fails on the default GC
  var i = 0
  doAssert impl(i, (inc i; i), (inc i; i)) == (0, 1, 2)

block use_temporary:
  ## Using proc with temporary values also does not work. Returns `(2, 1, 2)`
  ##
  ## Fails on both default and ARC/ORC GC
  var i = 0
  proc arg(): int = inc i; i

  doAssert impl(i, arg(), arg()) == (0, 1, 2)