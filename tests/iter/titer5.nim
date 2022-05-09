discard """
  output: "abcxyz"
"""
# Test method call syntax for iterators:
import strutils

const lines = """abc  xyz"""

var actual = ""
for x in lines.split():
  actual.add(x)

#OUT abcxyz
echo actual
