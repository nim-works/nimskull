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

## While there's no return type to infer for templates and macros, a meta-
## type used as the return type still triggers the converter.

template templ(): seq =
  1

# the converter is applied on the template's result already
let v1 = templ()
doAssert v1 is seq[int]
doAssert v1 == @[1]

macro m(x: int): seq =
  result = x

let v2 = m(1)
doAssert v2 is seq[int]
doAssert v2 == @[1]