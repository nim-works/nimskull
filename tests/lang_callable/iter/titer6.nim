discard """
  output: ""
"""
# Test iterator with more than 1 yield statement

import std/strutils

iterator tokenize2(s: string, seps: set[char] = Whitespace): tuple[
  token: string, isSep: bool] =
  var i = 0
  while i < s.len:
    var j = i
    if s[j] in seps:
      while j < s.len and s[j] in seps: inc(j)
      if j > i:
        yield (substr(s, i, j-1), true)
    else:
      while j < s.len and s[j] notin seps: inc(j)
      if j > i:
        yield (substr(s, i, j-1), false)
    i = j

var output = ""
for word, isSep in tokenize2("ta da", WhiteSpace):
  var titer2TestVar = 0
  output.add($titer2TestVar)

proc wordWrap2(s: string, maxLineWidth = 80,
               splitLongWords = true,
               seps: set[char] = Whitespace,
               newLine = "\n"): string  =
  result = ""
  for word, isSep in tokenize2(s, seps):
    var w = 0

output.add "\n"

doAssert output == "000\n"