discard """
  targets: c js vm
  knownIssue.js: "The new characters are not set to NUL"
"""

var a: string
a.setLen(3)
doAssert a == "\0\0\0"

# start from a constant string
var b = "abcdef"
b.setLen(3)
doAssert b == "abc"
b.setLen(6) # grow to previous size
doAssert b == "abc\0\0\0"

# start from a manually constructed string
var c: string
for _ in 0..<4:
  c.add 'c'
c.setLen(2)
doAssert c == "cc"
c.setLen(4) # grow to previous size
doAssert c == "cc\0\0"
