discard """
  output: ""
"""
# Test method call syntax for iterators:
import strutils

const lines = """abc  xyz"""

var output = ""
for x in lines.split():
  output.add(x)

#OUT abcxyz
output.add "\n"

doAssert output == "abcxyz\n"