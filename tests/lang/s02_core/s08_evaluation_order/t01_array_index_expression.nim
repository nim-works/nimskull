discard """
  description: '''
    Describe the evaluation order within array/seq/string/cstring/
    UncheckedArray accessor expressions
  '''
"""

# same as everywhere else, evaluation order is left-to-right, inside-to-
# outside
var simple = [0]
doAssert simple[(simple[0] = 1; 0)] == 1
# the effects of the index operand are fully computed before the result of the
# access operation is computed

# the effects of the innermore bracketed expression's index operand are fully
# computed before the outermore expression's index operand
var simple2 = [[0, 1], [2, 3]]
doAssert simple2[(simple2[1][0] = 1; 1)][simple2[1][0]] == 3

# the same is true for modification that don't affect the array itself
var i = 0
doAssert simple2[(inc i; 1)][i] == 3 # the accessed element is `simple2[1][1]`

i = 0
doAssert simple2[i][(inc i; 1)] == 1 # the accessed element is `simple2[0][1]`

proc get(): int =
  inc i
  result = i

i = 0
doAssert simple2[i][get()] == 1 # the accessed element is `simple2[0][1]`