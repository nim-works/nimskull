discard """
description: '''
  Test for call AST corruption where typing an argument which fails, and then
  is reused in an untyped context results in losing the original untyped AST.
'''
"""

# ``test`` has to be an overloaded routine, where the overload with the non-
# untyped parameter has to be defined **first**

proc test(arg: string) =
  discard

template test(arg: untyped) =
  for it in arg:
    discard

# two overloads of ``test2`` have to exist, with one being an iterator while
# the other is not. The return types don't matter as long as the non-iterator
# doesn't return anything that matches `string` or something for which an
# ``items`` iterator exists.
iterator test2(): int =
  yield 0

proc test2(): bool =
  discard

test(test2())