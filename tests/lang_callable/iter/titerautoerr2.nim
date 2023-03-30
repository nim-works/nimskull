discard """
  target: "!js !vm"
  errormsg: "type mismatch: got <string> but expected 'int literal(1)'"
  line: 11
"""

# JS and VM targets disabled until they support closure iterators (knownIssue)

iterator b(): auto {.closure.} =
  yield 1
  if true: return "str"
