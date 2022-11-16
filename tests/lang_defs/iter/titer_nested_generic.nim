discard """
  description: '''
  . From https://github.com/nim-lang/Nim/issues/1550
    Passing a generic iterator to another iterator doesn't seem to work
  . Original codes are not correct: ...
    It should give another parameters(int type) to z, which
    works since Nim v1.2.6
  '''
"""
type
  A[T] = iterator(x: T): T {.gcsafe, closure.}

iterator aimp[T](x: T): T {.gcsafe, closure.} =
  var total = 0
  while (total < 100):
    yield total
    total += x

iterator bimp(y: A[int], z:int): int {.gcsafe, closure.} =
  for i in y(z):
    yield i

for x in aimp[int](3):
  discard x

var y = aimp[int]
var z = bimp
for x in z(y, 1):
  discard x
  