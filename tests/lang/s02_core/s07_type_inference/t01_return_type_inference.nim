discard """
  description: '''
    The specified return type of non-generic routines may be generic, in
    which case the concrete type is inferred from the body.
  '''
"""

## Using 'auto' as the return type enables unconstrained return type
## inference. The real return type is taken from the source expression
## of the first analysed assignment to the `result` variable.

proc p(): auto =
  result = 1

doAssert typeof(p()) is int

## The return type specified on the procedure definition may also be a
## type-class of a built-in generic type, in which case it acts as a
## constraint.

proc p2(): tuple =
  result = (1, 2)

doAssert typeof(p2()) is (int, int)

## Return type inference is also available with iterators.

iterator iter(): auto =
  yield 1

doAssert typeof(iter()) is int

iterator iter2(): tuple =
  yield (1, 2)

doAssert typeof(iter2()) is (int, int)