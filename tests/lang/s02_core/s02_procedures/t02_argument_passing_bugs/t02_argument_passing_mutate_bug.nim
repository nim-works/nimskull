discard """
description: '''
Mutating variable while passing it to the procedure
arguments works differently on different backends.
'''
"""

# XXX: change the test to be more a proper specification test

proc impl(a, b, c: int): (int, int, int) = (a, b, c)

block use_temporary:
  var i = 0
  let a = i
  let b = (inc i; i)
  let c = (inc i; i)
  doAssert impl(a, b, c) == (0, 1, 2)




block fail_to_mutate_argument:
  var i = 0
  doAssert impl(i, (inc i; i), (inc i; i)) == (0, 1, 2)

block use_temporary:
  var i = 0
  proc arg(): int = inc i; i

  doAssert impl(i, arg(), arg()) == (0, 1, 2)