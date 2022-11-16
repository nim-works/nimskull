discard """
  output: "thisanexample"
"""
# Test an idea I recently had:

import macros

macro quoteWords(n: varargs[untyped]): untyped =
  result = newNimNode(nnkBracket)
  for i in 0 ..< n.len:
    expectKind(n[i], nnkIdent)
    result.add(toStrLit(n[i]))

const
  myWordList = quoteWords(this, an, example)

var s = ""
for w in items(myWordList):
  s.add(w)

echo s #OUT thisanexample
