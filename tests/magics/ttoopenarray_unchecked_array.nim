discard """
  description: '''
    Ensure that `toOpenArray` works for a pointer-to-UncheckedArray operand.
  '''
  targets: "c js vm"
  knownIssue.js: "the overload is not available"
  knownIssue.vm: "UncheckedArray is not supported"
"""

proc toSeq(a: openArray[int]): seq[int] {.noinline.} =
  ## Intended to prevent comparisons being folded away by the compiler.
  for x in a.items:
    result.add x

proc first(a: openArray[int]): int =
  toSeq(toOpenArray(a, 0, 0))[0]

let
  s = @[1, 2, 3, 4, 5]
  p = cast[ptr UncheckedArray[int]](addr s[0])

doAssert first(toOpenArray(p, 1, 3)) == 2
