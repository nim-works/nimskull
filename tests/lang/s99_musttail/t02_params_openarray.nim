discard """
  description: '''
    `.musttail` routines may use openarray parameters, with the same
    restrictions as var parameters
  '''
"""

proc tail1(a: openArray[int]): int {.musttail.} =
  a[0]

proc tail2(a: openArray[int]): int {.musttail.} =
  tail1(toOpenArray(a, 1, 1))

proc test1(a: array[2, int], pick: bool): int =
  if pick: tail2(a)
  else:    tail1(a)

doAssert test1([1, 2], false) == 1
doAssert test1([1, 2], true)  == 2

# mutable openarrays work too:

proc tail1m(a: var openArray[int]) {.musttail.} =
  a[0] = 4

proc tail2m(a: var openArray[int]) {.musttail.} =
  tail_1m(toOpenArray(a, 1, 1))

proc test2(a: var array[2, int], pick: bool) =
  if pick: tail2m(a)
  else:    tail1m(a)

var a = [1, 2]
test2(a, false)
echo a
doAssert a == [4, 2]

test2(a, true)
doAssert a == [4, 4]
