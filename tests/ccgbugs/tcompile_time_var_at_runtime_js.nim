discard """
  output: "1\n2\n2\n3"
  target: js
  knownIssue: "fails to compile because of symbol gen issues"
"""
var a {.compileTime.} = 1

echo a
a = 2
echo a
echo a
a = 3
echo a 