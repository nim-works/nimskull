discard """
  target: "!js !vm"
  output: '''5
14
0'''
"""

# JS and VM targets disabled until they support closure iterators (knownIssue)

iterator count[T](x: T, skip: bool): int {.closure.} =
  if skip: return x+10
  else: yield x+1

  if skip: return x+10
  else: yield x+2

proc takeProc[T](x: iterator (x: T, skip: bool): int) =
  echo x(4, false)
  echo x(4, true)
  echo x(4, false)

takeProc(count[int])

