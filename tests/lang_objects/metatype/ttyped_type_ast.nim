discard """
  description: "Ensure no untyped AST slips through to later compiler stages"
  action: compile
  knownIssue: '''When the typeof is analyzed by semTypeNode due to elementType
having a typedesc return type, the typed AST is discarded because semTypeNode
only returns a PType and thus the AST passed to isTyped stays untyped.
  '''
"""

import macros

template elementType*(a: untyped): typedesc =
  typeof(block: (for ai in a: ai))

iterator myiter3(): int = yield 10

proc collectSyms(n: NimNode): seq[NimNode] =
  if n.kind == nnkSym:
    result = @[n]
  else:
    for c in n:
      result.add collectSyms(c)

macro isTyped(a: typed): bool =
  newLit(collectSyms(a).len == 3)

static:
  assert: isTyped:
    typeof(block: (for ai in myiter3(): ai))

  # knownIssue; see above
  assert: isTyped:
    elementType(myiter3())
