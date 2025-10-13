discard """
  description: '''
    Ensure that the return-value optimization (=RVO) is disabled where it would
    affect observable semantics
  '''
"""

block pointer_dereferences:
  # tests for RVO combined with pointer dereferences. Arrays are used because, at
  # the time of writing, they're guaranteed to use the RVO
  var arr = [[1, 2]]
  let p = addr arr

  block:
    # test case: the argument is part of the destination and both lvalues come
    # from pointer dereferences
    proc get(x: array[2, int]): array[1, array[2, int]] =
      result = [[0, 0]]
      result = [x]

    p[] = get(p[][0])
    doAssert arr == [[1, 2]], "rvo was used"

  block:
    # test case: the destination is part of the argument and both lvalues come
    # from pointer dereferences
    proc get(x: array[1, array[2, int]]): array[2, int] =
      result = [0, 0]
      result = x[0]

    p[][0] = get(p[])
    doAssert arr == [[1, 2]], "rvo was used"

block implicit_openarray_conversion:
  # test case: whether to use RVO must also consider implicit to-openaArray
  # conversions
  var arr = [[1, 2]]

  proc get(x: openArray[array[2, int]]): array[2, int] =
    result = [0, 0]
    result = x[0]

  arr[0] = get(arr) # the destination is accessible through the argument
  doAssert arr == [[1, 2]], "rvo was used"
