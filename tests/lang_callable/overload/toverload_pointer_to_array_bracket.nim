discard """
  description: '''
    Regression test for pointer-to-array and pointer-to-tuple bracket
    overloads being ignored
  '''
"""

var
  arr = [1]
  tup = (2,)
  arrPtr = addr arr
  tupPtr = addr tup

# test bracket assignment overload for pointer-to-array and pointer-to-tuple
# types

template `[]=`(x: ptr array[1, int], y: string, val: int) =
  # `y`'s value is not relevant. It's only important that the parameter has a
  # type that's incompatible with the array's index type
  x[][0] = val

arrPtr[""] = 2

template `[]=`(x: ptr tuple[v: int], y: string, val: int) =
  x[][0] = val

tupPtr[""] = 3

# test bracket overload for pointer-to-array and pointer-to-tuple types

template `[]`(x: ptr array[1, int], y: string): int =
  x[][0]

doAssert arrPtr[""] == 2

template `[]`(x: ptr tuple[v: int], y: string): int =
  x[][0]

doAssert tupPtr[""] == 3
