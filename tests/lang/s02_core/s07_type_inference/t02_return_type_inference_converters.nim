discard """
  description: '''
    Generic converters are considered when inferring the return type.
  '''
"""

converter convert[T](x: T): seq[T] =
  @[x]

proc p(): seq =
  result = 1

doAssert p() == @[1]

## The same is true for return-type inference in iterators.

iterator iter(): seq =
  yield 1

for it in iter():
  doAssert it == @[1]