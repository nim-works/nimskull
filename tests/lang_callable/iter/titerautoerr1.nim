discard """
  target: "!js !vm"
  errormsg: "type mismatch: got <int literal(1)> but expected 'string'"
  line: 11
"""

# JS and VM targets disabled until they support closure iterators (knownIssue)

iterator a(): auto {.closure.} =
  if true: return "str"
  yield 1
