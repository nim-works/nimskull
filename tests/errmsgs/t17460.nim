discard """
  cmd: "nim check --msgFormat=sexp --filenames=canonical $options $file"
  nimoutFormat: sexp
  action: reject
"""

iterator xclusters*[T](a: openarray[T]; s: static[int]): array[s, T] {.inline.} =
  var result: array[s, T] # iterators have no default result variable
  var i = 0
  while i < len(a):
    for j, x in mpairs(result):
      x = a[(i + j) mod len(a)]
    yield result
    inc(i)

proc m =
  for (i, j, k) in xclusters([1, 2, 3, 4, 5], 3): #[tt.Error
     ^ (SemWrongNumberOfVariables) ]#
    echo i, j, k

m()