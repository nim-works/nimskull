discard """
  targets: c js vm
  description: '''
    Regression test for ``toOpenArray`` calls not bound-checking dereferenced
    pointer-to-array values
  '''
  knownIssue.vm: "`toOpenArray` is not yet supported"
"""

template check(x: untyped) =
  {.line.}:
    doAssertRaises IndexDefect:
      discard toOpenArray(x, 2, 3)

var arr = [0, 1]

# test with ref-of-array:
let a = new array[2, int]
check a[]

# test with ptr-of-array:
let p = addr arr
check p[]

# test with var-array:
proc getVar(x: var array[2, int]): var array[2, int] =
  result = x

check getVar(arr)

# test with lent-array:
proc getLent(x: array[2, int]): lent array[2, int] =
  result = x

check getLent(arr)