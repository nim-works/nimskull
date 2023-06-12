discard """
  description: '''
    Ensure closure and inline iterators have the same behaviour regarding
    parameter passing
  '''
  knownIssue: '''
    For inline iterators, the arguments are evaluated once, while
    for closure iterator they're evaluated on each iteration
  '''
"""


iterator clo(a: int): int {.closure.} =
  yield 0+a
  yield 1+a
  yield 2+a

iterator inl(a: int): int {.inline.} =
  yield 0+a
  yield 1+a
  yield 2+a

proc main =
  var
    y = 4
    elems: seq[int]

  for i in clo(y):
    elems.add i
    inc y

  doAssert elems == [4, 5, 6]

  # -----

  y = 4
  elems = @[]

  for i in inl(y):
    elems.add i
    inc y

  doAssert elems == [4, 5, 6]

main()
